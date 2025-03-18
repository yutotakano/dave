{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Dave.Raw.Version where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as Cpp

import Dave.Types


C.context (Cpp.cppCtx <> C.bsCtx <> daveContext)

C.include "dave/version.h"

Cpp.using "namespace discord::dave"

-- | Get the maximum supported Dave protocol version by this library.
maxSupportedProtocolVersion :: MonadIO m => m DaveProtocolVersion
maxSupportedProtocolVersion = liftIO $ do
    DaveProtocolVersion <$> [C.block|
        ProtocolVersion {
            return MaxSupportedProtocolVersion();
        }
    |]
