module Main
  ( main
  ) where

import RIO

import Data.Conduit (runConduit, (.|))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Types (status200, status500)
import Network.Wai (responseBuilder, responseLBS, Response)
import Network.Wai.Conduit (sourceRequestBody)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)

import Tomato.App (runApp)
import Tomato.Bot (tomatoBot)
import Tomato.Data.Except (DecodeException (..))
import Tomato.Data.Message (InMessage (..))
import Tomato.Validate (isTomato)

import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T

runServer :: Int -> IO ()
runServer port = run port $ \request send -> do
    eres <- tryAnyDeep $ do
      val <- runConduit
           $ sourceRequestBody request
          .| sinkParser Ae.json
      case Ae.fromJSON val of
          Ae.Success r -> return r
          Ae.Error s -> throwM $ DecodeException $ T.pack s
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
