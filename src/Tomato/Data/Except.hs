module Tomato.Data.Except
    ( DecodeException (..)
    ) where

import RIO

data DecodeException = DecodeException !Text
  deriving (Show, Typeable)
instance Exception DecodeException
