module Tomato.Post
    ( postTomato
    , uploadTomato
    , postMessage
    ) where

import RIO

import Network.HTTP.Req
    ( jsonResponse
    , ignoreResponse
    , req
    , ReqBodyFile (..)
    , ReqBodyJson (..)
    , (/:)
    , POST (..)
    , Req
    , https
    , header
    , responseBody
    )

import Data.Message (GMIUrl (..), OutMessage (..))

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Ae

-- TODO global config reader monad
tomatoFile :: FilePath
tomatoFile = "tomato.png"

accessToken :: ByteString
accessToken = "nj8X8tB3TC0MoZdMLhIUA4G89r9IlQfVej97Mhg3"

botId :: Text
botId = "713bcb4a4604006c944804552c"

-- | Post the current tomato
postTomato :: Req ()
postTomato = do
    url <- uploadTomato
    postMessage $ OutMessage url

-- | Upload the current tomato to the GroupMe image server
uploadTomato :: Req GMIUrl
uploadTomato = do
    let url = https "image.groupme.com" /: "pictures"
    js <- req POST url (ReqBodyFile tomatoFile) jsonResponse $
        header "X-Access-Token" accessToken <>
        header "Content-Type" "image/png"
    gmURL <- case Ae.fromJSON (responseBody js) of
        Ae.Success r -> pure r
        Ae.Error _s  -> error "deal with this later" -- TODO error monad
    return gmURL

-- | Post a message to the group with image as attachment
postMessage :: OutMessage -> Req ()
postMessage outMes = do
    let url = https "api.groupme.com" /: "v3" /: "bots" /: "post"
        outMes' = case Ae.toJSON outMes of
            Ae.Object o -> Ae.Object $ Ae.insert "bot_id" (Ae.toJSON botId) o
            _           -> error "exceptional"
    req POST url (ReqBodyJson outMes') ignoreResponse $
        header "X-Access-Token" accessToken
    return ()
