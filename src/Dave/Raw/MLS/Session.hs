{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Dave.Raw.MLS.Session where

import Control.Monad ( forM )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.IO.Unlift ( MonadUnliftIO( withRunInIO ) )
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Word ( Word16, Word64 )
import Foreign.Marshal.Array ( withArrayLen )
import Foreign.Marshal.Utils ( withMany )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Storable ( peekElemOff )
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
    (ptr, n) <- C.withPtr $ \p ->
        [C.block|
            uint16_t {
                std::vector<uint8_t> key = $(mls::Session* session)->GetLastEpochAuthenticator();

                // Copy the vector to a C array on heap so the data stays valid
                // even after we return to Haskell. It has to be freed manually
                // afterwards!!
                char** out_p = $(char** p);
                *out_p = static_cast<char *>(malloc(key.size()));
                memcpy(*out_p, key.data(), key.size());
                return key.size();
            }
        |]
    bs <- BS.packCStringLen (ptr, fromIntegral n)
    [C.block| void { free($(char* ptr)); } |]
    pure bs

-- | Set external sender using a marshalled representation of the sender.
-- TODO: untested
setExternalSender :: MonadIO m => MLSSession -> BS.ByteString -> m ()
setExternalSender session marshalledExternalSender = liftIO $ do
    BS.useAsCStringLen marshalledExternalSender $ \(p, n) -> do
        let n_size_t = fromIntegral n
        [C.block|
            void {
                // SetExternalVector wants a vector<uint8_t>, so first cast the
                // pointer, then copy it into a temporary vector
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
    -- Allocate an out pointer
    (ptr, n) <- C.withPtr $ \p ->
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
                    [C.block|
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


                            // Copy the vector to a C array on heap so the data stays valid
                            // even after we return to Haskell. It has to be freed manually
                            // afterwards!!
                            char** out_p = $(char** p);
                            if (commit_welcome.has_value())
                            {
                                *out_p = static_cast<char *>(malloc(commit_welcome.value().size()));
                                memcpy(*out_p, commit_welcome.value().data(), commit_welcome.value().size());
                                return commit_welcome.value().size();
                            }

                            *out_p = nullptr;
                            return 0;
                        }
                    |]
    -- Check if the output is a nullptr or a pointer to the
    -- C++ heap containing the bytestring
    if ptr == nullPtr then
        pure Nothing
    else do
        bs <- BS.packCStringLen (ptr, fromIntegral n)
        -- Free!
        [C.block| void { free($(char* ptr)); } |]
        pure $ Just bs

-- | Process a commit message and return either a hard reject (caller should
-- reset state), soft reject (caller should ignore the error) or a roster map
-- of user IDs to keys. The roster map lists IDs whose keys have been added,
-- changed, or removed; an empty value value means a key was removed.
-- TODO: untested
processCommit :: MonadIO m => MLSSession -> BS.ByteString -> m RosterVariant
processCommit session commitMessage = liftIO $ do
    -- Pass five out pointers:
    -- typ: 0 for RosterMap, 1 for failed_t, 2 for ignored_t
    -- key_p: Array of uint64_t keys for RosterMap
    -- val_p: Array of byte string values for RosterMap
    -- val_len_p: Array of lengths of byte strings in val_p
    -- n: Number of items in the key_p and val_p array
    (typ, key_p, val_p, val_len_p, n) <- C.withPtrs_ $ \(out_typ_p, out_key_p, out_value_p, out_value_len_p, out_n) ->
        BS.useAsCStringLen commitMessage $ \(commit_p, commit_n) -> do
            let commit_n_size_t = fromIntegral commit_n
            [C.block|
                void {
                    // Convert bytestring passed as C pointer + length
                    // into a std::vector<uint8_t>
                    uint8_t* ptr = (uint8_t*)$(char* commit_p);
                    const std::vector<uint8_t> commit(ptr, ptr + $(size_t commit_n_size_t));

                    RosterVariant v = $(mls::Session* session)->ProcessCommit(commit);

                    // RosterVariant is a std::variant of RosterMap, failed_t
                    // or ignored_t
                    uint8_t* out_typ_p = $(uint8_t* out_typ_p);
                    if (std::holds_alternative<failed_t>(v))
                    {
                        *out_typ_p = 1;
                        return;
                    }
                    else if (std::holds_alternative<ignored_t>(v))
                    {
                        *out_typ_p = 2;
                        return;
                    }

                    *out_typ_p = 0;
                    RosterMap m = std::get<RosterMap>(v);

                    // Set output size
                    *$(uint8_t* out_n) = m.size();

                    // Create C-style arrays on the heap for keys and values.
                    // (values are pointers to bytes)
                    // These need to be freed explicitly afterwards!
                    uint64_t** keys_p = $(uint64_t** out_key_p);
                    *keys_p = (uint64_t*)malloc(sizeof(uint64_t) * m.size());
                    char*** values_p = $(char*** out_value_p);
                    *values_p = (char**)malloc(sizeof(char*) * m.size());
                    uint8_t** value_lens_p = $(uint8_t** out_value_len_p);
                    *value_lens_p = (uint8_t*)malloc(sizeof(uint8_t*) * m.size());

                    // Insert into above array from the kv map
                    int i = 0;
                    for (const auto &[key, value] : m)
                    {
                        (*keys_p)[i] = key;
                        (*value_lens_p)[i] = value.size();
                        (*values_p)[i] = (char*)malloc(value.size());
                        memcpy((*values_p)[i], value.data(), value.size());
                    }
                }
            |]
    case typ of
        1 -> pure HardReject
        2 -> pure SoftReject
        _ -> do
            -- Create a list of (key,value) pairs
            pairs <- forM [0..(fromIntegral n - 1)] $ \i -> do
                key <- peekElemOff key_p i
                val <- peekElemOff val_p i
                val_len <- peekElemOff val_len_p i
                bs <- BS.packCStringLen (val, fromIntegral val_len)
                pure (key, bs)

            -- Make sure we free all dynamically allocated arrays
            [C.block|
                void {
                    free($(uint64_t* key_p));
                    char** val_p = $(char** val_p);
                    for (int i = 0; i < $(uint8_t n); i++)
                    {
                        free(val_p[i]);
                    }
                    free(val_p);
                    free($(uint8_t* val_len_p));
                }
            |]
            pure $ RosterMap $ Map.fromList pairs

processWelcome :: MonadIO m => MLSSession -> BS.ByteString -> [BS.ByteString] -> m (Maybe RosterMap)
processWelcome session welcomeMessage recognizedUserIDs = liftIO $ do
    -- Pass four out pointers:
    -- key_p: Array of uint64_t keys for RosterMap - nullPtr indicates failure
    -- val_p: Array of byte string values for RosterMap
    -- val_len_p: Array of lengths of byte strings in val_p
    -- n: Number of items in the key_p and val_p array
    (key_p, val_p, val_len_p, n) <- C.withPtrs_ $ \(out_key_p, out_value_p, out_value_len_p, out_n) ->
        -- Get welcome message bytestring as a c string with length
        BS.useAsCStringLen welcomeMessage $ \(welcome_ptr, welcome_n) -> do
            -- Wrap length (Int) with fromIntegral so we can pass it as size_t
            let welcome_n_size_t = fromIntegral welcome_n
            -- Nest useAsCString for all user ID bytestrings
            withMany BS.useAsCString recognizedUserIDs $ \cstrs -> do
                -- Create a pointer for the list of user ID c strings
                withArrayLen cstrs $ \str_n str_ptr -> do
                    -- Wrap length (Int) with fromIntegral so it is size_t
                    let str_n_32 = fromIntegral str_n
                    [C.block|
                        void {
                            // Convert bytestring passed as C pointer + length
                            // into a std::vector<uint8_t>
                            uint8_t* ptr = (uint8_t*)$(char* welcome_ptr);
                            const std::vector<uint8_t> welcome(ptr, ptr + $(size_t welcome_n_size_t));

                            // Convert array of strings (null-terminated C strs)
                            // and the array length into a std::set<std::string>
                            std::set<std::string> user_ids;
                            char** cstrs = $(char** str_ptr);
                            for (size_t i = 0; i < $(int32_t str_n_32); i++)
                            {
                                user_ids.insert(std::string(cstrs[i]));
                            }

                            std::optional<RosterMap> v = $(mls::Session* session)->ProcessWelcome(welcome, user_ids);

                            uint64_t** keys_p = $(uint64_t** out_key_p);
                            if (!v.has_value())
                            {
                                *keys_p = nullptr;
                                return;
                            }

                            RosterMap m = v.value();

                            // Set output size
                            *$(uint8_t* out_n) = m.size();

                            // Create C-style arrays on the heap for keys and values.
                            // (values are pointers to bytes)
                            // These need to be freed explicitly afterwards!
                            *keys_p = (uint64_t*)malloc(sizeof(uint64_t) * m.size());
                            char*** values_p = $(char*** out_value_p);
                            *values_p = (char**)malloc(sizeof(char*) * m.size());
                            uint8_t** value_lens_p = $(uint8_t** out_value_len_p);
                            *value_lens_p = (uint8_t*)malloc(sizeof(uint8_t*) * m.size());

                            // Insert into above array from the kv map
                            int i = 0;
                            for (const auto &[key, value] : m)
                            {
                                (*keys_p)[i] = key;
                                (*value_lens_p)[i] = value.size();
                                (*values_p)[i] = (char*)malloc(value.size());
                                memcpy((*values_p)[i], value.data(), value.size());
                            }
                        }
                    |]
    if key_p == nullPtr then
        pure Nothing
    else do
        -- Create a list of (key,value) pairs
        pairs <- forM [0..(fromIntegral n - 1)] $ \i -> do
            key <- peekElemOff key_p i
            val <- peekElemOff val_p i
            val_len <- peekElemOff val_len_p i
            bs <- BS.packCStringLen (val, fromIntegral val_len)
            pure (key, bs)

        -- Make sure we free all dynamically allocated arrays
        [C.block|
            void {
                free($(uint64_t* key_p));
                char** val_p = $(char** val_p);
                for (int i = 0; i < $(uint8_t n); i++)
                {
                    free(val_p[i]);
                }
                free(val_p);
                free($(uint8_t* val_len_p));
            }
        |]
        pure $ Just $ Map.fromList pairs

-- | Get the generated key package as a marshalled strict bytestring.
-- This must be called after 'init'.
-- TODO: untested
getMarshalledKeyPackage :: MonadIO m => MLSSession -> m BS.ByteString
getMarshalledKeyPackage session = liftIO $ do
    (ptr, n) <- C.withPtr $ \p ->
        [C.block|
            uint16_t {
                std::vector<uint8_t> key = $(mls::Session* session)->GetMarshalledKeyPackage();

                // Copy the vector to a C array on heap so the data stays valid
                // even after we return to Haskell. It has to be freed manually
                // afterwards!!
                char** out_p = $(char** p);
                *out_p = static_cast<char *>(malloc(key.size()));
                memcpy(*out_p, key.data(), key.size());
                return key.size();
            }
        |]
    bs <- BS.packCStringLen (ptr, fromIntegral n)
    [C.block| void { free($(char* ptr)); } |]
    pure bs

-- | Get the key ratchet for a user ID.
-- TODO: untested
getKeyRatchet :: MonadIO m => MLSSession -> BS.ByteString -> m (Maybe KeyRatchet)
getKeyRatchet session userId = liftIO $ do
    res <- BS.useAsCString userId $ \cstr ->
        [C.block|
            IKeyRatchet* {
                char* cstr = $(char* cstr);
                std::string userId(cstr);
                auto ratchet = $(mls::Session* session)->GetKeyRatchet(userId);
                if (ratchet == nullptr) return nullptr;

                // Release unique_ptr to raw pointer: must be freed explicitly!
                return ratchet.release();
            }
        |]
    if res == nullPtr then
        pure Nothing
    else
        pure $ Just res

-- | Derive a pairwise fingerprint key (using scrypt from OpenSSL) using the
-- combined hash for our own and the provided user ID. The provided callback
-- will be run in a separate thread once the key is derived.
getPairwiseFingerprint :: MonadUnliftIO m => MLSSession -> Word16 -> BS.ByteString -> (BS.ByteString -> m ()) -> m ()
getPairwiseFingerprint session version userId callback = withRunInIO $ \unliftIO -> do
    -- Create a wrapper callback function to free the C array after copy
    let callbackCleanup ptr n = do
            bs <- BS.packCStringLen (ptr, fromIntegral n)
            [C.block| void { free($(char* ptr)); } |]
            unliftIO $ callback bs
    callbackPtr <- $(C.mkFunPtr [t| Ptr C.CChar -> C.CSize -> IO () |]) callbackCleanup

    BS.useAsCString userId $ \cstr -> do
        [C.block|
            void {
                // Wrap Haskell callback with a C++ function for some conversion
                // from vectors to C-style array + length.
                std::function<void(std::vector<uint8_t> const&)> cppCallback([&](std::vector<uint8_t> vec)
                {
                    // Needs explicit free afterwards!
                    char* out_p = static_cast<char *>(malloc(vec.size()));
                    memcpy(out_p, vec.data(), vec.size());
                    $(void (*callbackPtr)(char *, size_t))(out_p, vec.size());
                });

                std::string userId($(char* cstr));
                $(mls::Session* session)->GetPairwiseFingerprint($(uint16_t version), userId, cppCallback);
            }
        |]
