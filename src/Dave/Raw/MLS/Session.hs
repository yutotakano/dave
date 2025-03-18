{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Dave.Raw.MLS.Session where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Word ( Word64 )
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as Cpp

import Dave.Types

C.context (Cpp.cppCtx <> C.bsCtx <> daveContext)

C.include "dave/mls/session.h"

Cpp.using "namespace discord::dave"

-- | Create a new @MLSSession@ and return an opaque reference to it.
new :: MonadIO m => m MLSSession
new = liftIO $ do
    [C.block|
        mls::Session* {
            return new mls::Session(nullptr, "", [](std::string const &s1, std::string const &s2) {
                // TODO: we do nothing
            });
        }
    |]

-- |
-- TODO: untested - come back when SignaturePrivateKey can be created
-- TODO: undocumented
init :: MonadIO m => MLSSession -> DaveProtocolVersion -> Word64 -> Word64 -> SignaturePrivateKey -> m ()
init session (DaveProtocolVersion protocolVersion) groupId selfUserId transientKey = liftIO $ do
    [C.block|
        void {
            // libdave wants the self user ID as a const std::string, so convert
            std::string uid = std::to_string($(uint64_t selfUserId));

            // Straightforward passing of arguments, except for private key:
            // Since the Haskell side holds an opaque void pointer to a
            // std::shared_ptr (see upcoming documentation on the creation of
            // SignaturePrivateKey from within Haskell), we cast it and
            // dereference it before passing it to the function.
            $(mls::Session* session)->Init($(uint16_t protocolVersion), $(uint64_t groupId), uid, *(std::shared_ptr<::mlspp::SignaturePrivateKey> *)$(void* transientKey));
        }
    |]

-- | Reset the Dave MLS session entirely. You must call 'init' to use this
-- session again.
reset :: MonadIO m => MLSSession -> m ()
reset session = liftIO $ do
    [C.block|
        void {
            $(mls::Session* session)->Reset();
        }
    |]
