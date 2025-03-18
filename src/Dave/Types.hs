{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Dave.Types where

import Foreign.Ptr ( Ptr )
import Data.Word ( Word16 )

import Language.C.Inline qualified as C
import Language.C.Types qualified as CT
import Language.C.Inline.Cpp qualified as Cpp

-- | Opaque type for a C++ object of mls::Session.
data RawDaveMLSSession
-- | Type alias for a pointer to RawDaveMLSSession to simplify API. Construct
-- this using 'Dave.Raw.MLS.Session.new', then call 'Dave.Raw.MLS.Session.init'
-- on it to initialise it.
type MLSSession = Ptr RawDaveMLSSession

-- | Dave protocol version is represented as a uint16_t in libdave, but this is
-- an implementation detail so we also hide it behind a wrapper type in the
-- Haskell API too.
newtype DaveProtocolVersion = DaveProtocolVersion Word16
    deriving (Show, Read, Eq) via Word16


-- | Set up matching types between C++ return types and Haskell types
daveContext :: C.Context
daveContext = Cpp.cppTypePairs
    [ ( "mls::Session" :: CT.CIdentifier, [t| RawDaveMLSSession |] )
    , ( "ProtocolVersion" :: CT.CIdentifier, [t| Word16 |])
    ]
