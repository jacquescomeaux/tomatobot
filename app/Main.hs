module Main
  ( main
  ) where

import RIO

import Data.Conduit (runConduit, (.|))
import Data.Conduit.Attoparsec (sinkParser)
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
import Network.HTTP.Types (status200, status500)
import Network.Wai (responseBuilder, responseLBS, Response)
import Network.Wai.Conduit (sourceRequestBody)
import Network.Wai.Handler.Warp (run)

import Data.Message (InMessage (..))
import Data.Tomato (Tomato)
import Tomato.Post (postTomato)
import Tomato.Validate (isTomato)

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy.Char8 as BL8


data App = App
    { appLogFunc :: !LogFunc
    , appToken   :: !Text
    , appBotId   :: !Text
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
          , appToken = "placeholder"
          , appBotId = "placeholder"
          }
    runRIO app inner

_randomTomato :: Req Tomato
_randomTomato = do
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
  logInfo $ "Fetching tomato"
  -- tomato <- runReq defaultHttpConfig randomTomato
  -- logInfo $ displayShow tomato
  -- url <- runReq defaultHttpConfig postTomato
  runReq defaultHttpConfig postTomato
  -- logInfo $ displayShow url
  logInfo $ "All done"

main :: IO ()
main = run 3000 $ \request send -> do
    eres <- tryAnyDeep $ do
      val <- runConduit
           $ sourceRequestBody request
          .| sinkParser Ae.json
      case Ae.fromJSON val of
          Ae.Success r -> return r
          Ae.Error _s  -> error "handle this later"
    case eres of
      Left e -> send $ errorResponse e
      Right inMes -> do
          when (isTomato inMes) $ runApp tomatoBot
          send $ validResponse inMes
  where
    errorResponse :: SomeException -> Response
    errorResponse e = responseLBS
        status500
        [("Content-Type", "text/plain")]
        $ BL8.pack $ "Exception occurred: " ++ show e
    validResponse :: InMessage -> Response
    validResponse inMes = responseBuilder
        status200
        [("Content-Type", "application/json")]
        $ Ae.fromEncoding $ Ae.toEncoding $ text inMes
