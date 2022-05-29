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
    , Req
    , defaultHttpConfig
    , runReq
    , ReqBodyFile (..)
    , ReqBodyJson (..)
    , (/:)
    , POST (..)
    , Req
    , https
    , header
    , responseBody
    )

import Tomato.Data.Except (DecodeException (..))
import Tomato.Data.Message (GMIUrl (..), OutMessage (..))
import Tomato.App (App (..))

import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as Ae
import qualified RIO.Text as T


-- | Post the current tomato
postTomato :: RIO App ()
postTomato = do
    url <- uploadTomato
    postMessage $ OutMessage url

-- | Upload the current tomato to the GroupMe image server
uploadTomato :: RIO App GMIUrl
uploadTomato = do
    tomatoFile <- asks appFile
    accessToken <- asks appToken
    let url = https "image.groupme.com" /: "pictures"
    js <- rr $ req POST url (ReqBodyFile tomatoFile) jsonResponse $
        header "X-Access-Token" accessToken <>
        header "Content-Type" "image/png"
    gmURL <- case Ae.fromJSON (responseBody js) of
        Ae.Success r -> pure r
        Ae.Error s -> throwM $ DecodeException $ T.pack s
    return gmURL

-- | Post a message to the group with image as attachment
postMessage :: OutMessage -> RIO App ()
postMessage outMes = do
    botId <- asks appBotId
    accessToken <- asks appToken
    let url = https "api.groupme.com" /: "v3" /: "bots" /: "post"
        outMes' = case Ae.toJSON outMes of
            Ae.Object o -> Ae.Object $ Ae.insert "bot_id" (Ae.toJSON botId) o
            _           -> error "exceptional"
    rr $ req POST url (ReqBodyJson outMes') ignoreResponse $
        header "X-Access-Token" accessToken
    return ()

rr :: Req a -> RIO App a
rr = runReq defaultHttpConfig
