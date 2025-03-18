{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Dave.Types where

import Foreign.Ptr ( Ptr )
import Data.Word ( Word8, Word16 )

import Language.C.Inline qualified as C
import Language.C.Types qualified as CT
import Language.C.Inline.Cpp qualified as Cpp

-- | Opaque type for a C++ object of mls::Session.
data RawDaveMLSSession
-- | Type alias for a pointer to RawDaveMLSSession to simplify API. Construct
-- this using 'Dave.Raw.MLS.Session.new', then call 'Dave.Raw.MLS.Session.init'
-- on it to initialise it.
type MLSSession = Ptr RawDaveMLSSession

-- | A type representing the protocol version for DAVE.
--
-- Internally, libdave (in C++) represents this version as a uint16_t, so our
-- Haskell library also stores it as a 'Word16'. However, as this is an
-- implementation detail, we've hid it behind the newtype wrapper in the API.
newtype DaveProtocolVersion = DaveProtocolVersion Word16
    deriving (Show, Read, Eq) via Word16

-- | A type representing the signature version for DAVE.
--
-- Internally, libdave (in C++) represents this version as a uint8_t, so our
-- Haskell library also stores it as a 'Word8'. However, as this is an
-- implementation detail, we've hid it behind the newtype wrapper in the API.
newtype DaveSignatureVersion = DaveSignatureVersion Word8
    deriving (Show, Read, Eq) via Word8

-- | Set up matching types between C++ return types and Haskell types
daveContext :: C.Context
daveContext = Cpp.cppTypePairs
    [ ( "mls::Session" :: CT.CIdentifier, [t| RawDaveMLSSession |] )
    , ( "ProtocolVersion" :: CT.CIdentifier, [t| Word16 |])
    , ( "SignatureVersion" :: CT.CIdentifier, [t| Word8 |])
    ]
