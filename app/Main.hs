module Main
    ( main
    ) where

import RIO

import Network.HTTP.Req
    ( jsonResponse
    , req
    , (=:)
    , NoReqBody (..)
    , (/:)
    , GET (..)
    , https
    , responseBody
    , Req
    , defaultHttpConfig
    , runReq
    )

import Data.Tomato (Tomato)
import Tomato.Post (postTomato)

import qualified Data.Aeson.Types as Ae


data App = App
    { appLogFunc :: !LogFunc
    , appName :: !Utf8Builder
    , appToken :: !Text
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

runApp :: RIO App a -> IO a
runApp inner = do
  logOptions' <- logOptionsHandle stderr False
  -- let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  let logOptions = logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let app = App
          { appLogFunc = logFunc
          , appName = "Tomato Bot"
          , appToken =  "placeholder"
          }
    runRIO app inner

randomTomato :: Req Tomato
randomTomato = do
    let url = https "api.unsplash.com" /: "photos" /: "random"
    js <- req GET url NoReqBody jsonResponse $
        "query" =: ("tomato" :: Text) <>
        "client_id" =: ("FbzqI-oR7277JwL1ZGsyUw7yG1F5U0U3WhQ3kOW71Do" :: Text)
    tomato <- case Ae.fromJSON (responseBody js) of
        Ae.Success r -> pure r
        Ae.Error _s  -> error "deal with this later"
    return tomato

tomatoBot :: RIO App ()
tomatoBot = do
  name <- view $ to appName
  logInfo $ "Hello, " <> name
  logInfo $ "Fetching tomato"
  -- tomato <- runReq defaultHttpConfig randomTomato
  -- logInfo $ displayShow tomato
  -- url <- runReq defaultHttpConfig postTomato
  runReq defaultHttpConfig postTomato
  -- logInfo $ displayShow url
  logInfo $ "All done"

main :: IO ()
main = runApp tomatoBot
