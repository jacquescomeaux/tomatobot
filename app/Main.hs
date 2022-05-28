module Main
  ( main
  ) where

import RIO

import Data.Conduit (runConduit, (.|))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Req (defaultHttpConfig, runReq)
import Network.HTTP.Types (status200, status500)
import Network.Wai (responseBuilder, responseLBS, Response)
import Network.Wai.Conduit (sourceRequestBody)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)

import Data.Message (InMessage (..))
import Tomato.Post (postTomato)
import Tomato.Validate (isTomato)
import Tomato.Retrieve (randomTomato)

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified RIO.List.Partial as L'


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
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  -- let logOptions = logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let app = App
          { appLogFunc = logFunc
          , appToken = "placeholder"
          , appBotId = "placeholder"
          }
    runRIO app inner

tomatoBot :: RIO App ()
tomatoBot = do
  logInfo $ "Fetching tomato"
  runReq defaultHttpConfig randomTomato
  logInfo $ "Posting tomato"
  runReq defaultHttpConfig postTomato
  logInfo $ "Done"

runServer :: Int -> IO ()
runServer port = run port $ \request send -> do
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

main :: IO ()
main = do
    args <- getArgs
    when (null args) (error "no port")
    let portS = readMaybe $ L'.head args
    let port = maybe (error "invalid port") id portS
    runServer port
