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
    , NoReqBody (..)
    , (/:)
    , (=:)
    , Req
    , GET (..)
    , https
    , responseBody
    )

import Data.Tomato (Tomato (..), Links (..))

import qualified Data.Aeson as Ae
import qualified RIO.ByteString as B


-- TODO global config reader monad
tomatoFile :: FilePath
tomatoFile = "tomato.png"

clientId :: Text
clientId = "FbzqI-oR7277JwL1ZGsyUw7yG1F5U0U3WhQ3kOW71Do"

-- | Get a random tomato
randomTomato :: Req ()
randomTomato = queryTomato >>= downloadTomato

-- | Ask for a tomato
queryTomato :: Req Tomato
queryTomato = do
    let url = https "api.unsplash.com" /: "photos" /: "random"
    js <- req GET url NoReqBody jsonResponse $
        "query" =: ("tomato" :: Text) <>
        "client_id" =: clientId
    tomato <- case Ae.fromJSON (responseBody js) of
        Ae.Success r -> pure r
        Ae.Error _s  -> error "deal with this later"
    return tomato

-- | Download a specific tomato
downloadTomato :: Tomato -> Req ()
downloadTomato tom = do
    let url = download (links tom)
    bs <- req GET url NoReqBody bsResponse $
        "query" =: ("tomato" :: Text) <>
        "client_id" =: clientId
    B.writeFile tomatoFile (responseBody bs)
