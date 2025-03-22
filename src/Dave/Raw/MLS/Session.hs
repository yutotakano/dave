{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Dave.Raw.MLS.Session where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.ByteString qualified as BS
import Data.Word ( Word64 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArrayLen )
import Foreign.Marshal.Utils ( withMany )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( peek )
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
-- TODO: untested
reset :: MonadIO m => MLSSession -> m ()
reset session = liftIO $ do
    [C.block|
        void {
            $(mls::Session* session)->Reset();
        }
    |]

-- | Change the protocol version of a session after initialization.
-- TODO: untested
setProtocolVersion :: MonadIO m => MLSSession -> DaveProtocolVersion -> m ()
setProtocolVersion session (DaveProtocolVersion protocolVersion) = liftIO $ do
    [C.block|
        void {
            $(mls::Session* session)->SetProtocolVersion($(uint16_t protocolVersion));
        }
    |]

-- | Get the epoch authenticator.
-- TODO: untested
getLastEpochAuthenticator :: MonadIO m => MLSSession -> m BS.ByteString
getLastEpochAuthenticator session = liftIO $ do
    alloca $ \p -> do
        n <- [C.block|
            uint16_t {
                std::vector<uint8_t> key = $(mls::Session* session)->GetLastEpochAuthenticator();
                *$(char** p) = (char *)(key.data());
                return key.size();
            }
        |]
        peek p >>= \ptr -> BS.packCStringLen (ptr, fromIntegral n)

-- | Set external sender using a marshalled representation of the sender.
-- TODO: untested
setExternalSender :: MonadIO m => MLSSession -> BS.ByteString -> m ()
setExternalSender session marshalledExternalSender = liftIO $ do
    BS.useAsCStringLen marshalledExternalSender $ \(p, n) -> do
        let n_size_t = fromIntegral n
        [C.block|
            void {
                // SetExternalVector wants a vector<unsigned char>, so first
                // cast the pointer, then copy it into a temporary vector
                uint8_t* ptr = (uint8_t*)$(char* p);
                const std::vector<uint8_t> vec(ptr, ptr + $(size_t n_size_t));
                $(mls::Session* session)->SetExternalSender(vec);
            }
        |]

-- | Process proposals with a list of recognized user IDs. Returns the commit
-- and welcome messages in one buffer.
-- TODO: untested
processProposals :: MonadIO m => MLSSession -> BS.ByteString -> [BS.ByteString] -> m (Maybe BS.ByteString)
processProposals session proposals recognizedUserIDs = liftIO $ do
    -- Get proposals bytestring as a c string with length
    BS.useAsCStringLen proposals $ \(proposals_ptr, proposals_n) -> do
        -- Wrap length (Int) with fromIntegral so we can pass it as size_t
        let proposals_n_size_t = fromIntegral proposals_n
        -- Nest useAsCString for all user ID bytestrings
        withMany BS.useAsCString recognizedUserIDs $ \cstrs -> do
            -- Create a pointer for the list of user ID c strings
            withArrayLen cstrs $ \str_n str_ptr -> do
                -- Wrap length (Int) with fromIntegral so it is size_t
                let str_n_32 = fromIntegral str_n
                -- Allocate an out pointer
                alloca $ \out_p_p -> do
                    n <- [C.block|
                        uint16_t {
                            // Convert bytestring passed as C pointer + length
                            // into a std::vector<uint8_t>
                            uint8_t* ptr = (uint8_t*)$(char* proposals_ptr);
                            const std::vector<uint8_t> proposals(ptr, ptr + $(size_t proposals_n_size_t));

                            // Convert array of strings (null-terminated C strs)
                            // and the array length into a std::set<std::string>
                            std::set<std::string> user_ids;
                            char** cstrs = $(char** str_ptr);
                            for (size_t i = 0; i < $(int32_t str_n_32); i++)
                            {
                                user_ids.insert(std::string(cstrs[i]));
                            }

                            // Call ProcessProposals, which returns the combined
                            // commit and welcome message bytestream
                            std::optional<std::vector<uint8_t>> commit_welcome = $(mls::Session* session)->ProcessProposals(proposals, user_ids);

                            char** out_p_p = $(char** out_p_p);
                            if (commit_welcome.has_value())
                            {
                                *out_p_p = (char *)commit_welcome.value().data();
                                return commit_welcome.value().size();
                            }

                            *out_p_p = nullptr;
                            return 0;
                        }
                    |]
                    out_p <- peek out_p_p
                    if out_p == nullPtr then
                        pure Nothing
                    else
                        Just <$> BS.packCStringLen (out_p, fromIntegral n)

-- ProcessCommit
-- ProcessWelcome

-- | Get the generated key package as a marshalled strict bytestring.
-- This must be called after 'init'.
-- TODO: untested
getMarshalledKeyPackage :: MonadIO m => MLSSession -> m BS.ByteString
getMarshalledKeyPackage session = liftIO $ do
    alloca $ \p -> do
        n <- [C.block|
            uint16_t {
                std::vector<uint8_t> key = $(mls::Session* session)->GetMarshalledKeyPackage();
                *$(char** p) = (char *)(key.data());
                return key.size();
            }
        |]
        peek p >>= \ptr -> BS.packCStringLen (ptr, fromIntegral n)



-- GetKeyRatchet
-- GetPairwiseFingerprint
