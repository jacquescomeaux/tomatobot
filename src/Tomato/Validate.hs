module Tomato.Validate
    ( isTomato
    ) where

import RIO

import Data.Message (InMessage (..))


isTomato :: InMessage -> Bool
isTomato m =
    not (hasAttach m) &&
    groupId m == 87220147 && 
    isUser m &&
    text m == "tomato"

