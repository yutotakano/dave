{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Dave.Raw.Context where

import Language.C.Inline qualified as C
import Language.C.Types qualified as CT

import Language.C.Inline.Cpp qualified as Cpp

import Dave.Raw.Types

daveContext :: C.Context
daveContext = Cpp.cppTypePairs
    [ ( "mls::Session" :: CT.CIdentifier, [t| DaveMLSSession |] )
    ]
