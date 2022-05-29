module Tomato.Retrieve
    ( randomTomato
    , downloadTomato
    , queryTomato
    ) where

import RIO

import Network.HTTP.Req
    ( jsonResponse
    , bsResponse
    , req
    , Req
    , runReq
    , defaultHttpConfig
    , NoReqBody (..)
    , (/:)
    , (=:)
    , GET (..)
    , https
    , responseBody
    )

import Tomato.App (App (..))
import Tomato.Data.Except (DecodeException (..))
import Tomato.Data.Tomato (Tomato (..), Links (..))

import qualified Data.Aeson as Ae
import qualified RIO.ByteString as B
import qualified RIO.Text as T


-- | Get a random tomato
randomTomato :: RIO App ()
randomTomato = queryTomato >>= downloadTomato

-- | Ask for a tomato
queryTomato :: RIO App Tomato
queryTomato = do
    clientId <- asks appClientId
    let url = https "api.unsplash.com" /: "photos" /: "random"
    js <- rr $ req GET url NoReqBody jsonResponse $
        "query" =: ("tomato" :: Text) <>
        "client_id" =: clientId
    tomato <- case Ae.fromJSON (responseBody js) of
        Ae.Success r -> pure r
        Ae.Error s -> throwM $ DecodeException $ T.pack s
    return tomato

-- | Download a specific tomato
downloadTomato :: Tomato -> RIO App ()
downloadTomato tom = do
    tomatoFile <- asks appFile
    clientId <- asks appClientId
    let url = download (links tom)
    bs <- rr $ req GET url NoReqBody bsResponse $
        "query" =: ("tomato" :: Text) <>
        "client_id" =: clientId
    B.writeFile tomatoFile (responseBody bs)

rr :: Req a -> RIO App a
rr = runReq defaultHttpConfig
