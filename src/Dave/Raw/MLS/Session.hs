{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Dave.Raw.MLS.Session where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Foreign.Ptr ( Ptr )
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as Cpp

import Dave.Raw.Context
import Dave.Raw.Types

C.context (Cpp.cppCtx <> C.bsCtx <> daveContext)

C.include "dave/mls/session.h"

Cpp.using "namespace discord::dave"

type MLSSession = Ptr DaveMLSSession

new :: MonadIO m => m MLSSession
new = liftIO $ do
    [C.block|
        mls::Session* {
            return new mls::Session(nullptr, "", [](std::string const &s1, std::string const &s2) {
                // TODO: we do nothing
            });
        }
    |]
