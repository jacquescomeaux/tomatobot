module Tomato.Bot
  ( tomatoBot
  ) where

import RIO

import Tomato.App (App)
import Tomato.Post (postTomato)
import Tomato.Retrieve (randomTomato)


-- | Fetch and post a tomato
tomatoBot :: RIO App ()
tomatoBot = do
  logInfo $ "Fetching tomato"
  randomTomato
  logInfo $ "Posting tomato"
  postTomato
  logInfo $ "Done"
