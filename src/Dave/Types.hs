{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Dave.Types where

import Foreign.Ptr ( Ptr )

import Language.C.Inline qualified as C
import Language.C.Types qualified as CT
import Language.C.Inline.Cpp qualified as Cpp

-- | Opaque type for a C++ object of mls::Session.
data RawDaveMLSSession
-- | Type alias for a pointer to RawDaveMLSSession to simplify API. Construct
-- this using 'Dave.Raw.MLS.Session.new', then call 'Dave.Raw.MLS.Session.init'
-- on it to initialise it.
type MLSSession = Ptr RawDaveMLSSession

-- | Set up matching types between C++ return types and Haskell types
daveContext :: C.Context
daveContext = Cpp.cppTypePairs
    [ ( "mls::Session" :: CT.CIdentifier, [t| RawDaveMLSSession |] )
    ]
