module Util (ioFromMaybe) where

import Control.Exception (throwIO)
import Flow

ioFromMaybe :: Maybe a -> IO a
ioFromMaybe (Just a) = return a
ioFromMaybe _ = throwIO <| userError "empty"