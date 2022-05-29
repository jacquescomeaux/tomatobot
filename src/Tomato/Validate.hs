module Tomato.Validate
    ( isTomato
    ) where

import RIO

import Tomato.Data.Message (InMessage (..))


-- | Check if a message is a tomato request
isTomato :: InMessage -> Bool
isTomato m =
    not (hasAttach m) &&
    groupId m == "87220147" && 
    isUser m &&
    text m == "tomato"

