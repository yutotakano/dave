{-# LANGUAGE TemplateHaskell #-}
module Dave.Raw.MLS.Session where

import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as Cpp

C.context (Cpp.cppCtx <> C.bsCtx)

C.include "dave/mls/session.h"

