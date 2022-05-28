module Data.Tomato
    ( Links (..)
    , Tomato (..)
    , Urls (..)
    ) where

import RIO

import Data.Aeson ((.:))
import Network.HTTP.Req (Url, Scheme (Https), useHttpsURI)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import qualified Text.URI as URI


data Tomato = Tomato
    { id          :: !Text
    , width       :: !Word
    , height      :: !Word
    , color       :: !Text
    , blur_hash   :: !Text
    , description :: !Text
    , urls        :: !Urls
    , links       :: !Links
    } deriving Show

instance Ae.FromJSON Tomato where
    parseJSON = Ae.withObject "Tomato" $ \o -> Tomato
        <$> o .: "id"
        <*> o .: "width"
        <*> o .: "height"
        <*> o .: "color"
        <*> o .: "blur_hash"
        <*> o .: "description"
        <*> o .: "urls"
        <*> o .: "links"

data Links = Links
    { self              :: !(Url Https)
    , html              :: !(Url Https)
    , download          :: !(Url Https)
    , download_location :: !(Url Https)
    } deriving Show

instance Ae.FromJSON Links where
    parseJSON = Ae.withObject "Links" $ \o -> Links
        <$> (o .: "self" >>= toUrl)
        <*> (o .: "html" >>= toUrl)
        <*> (o .: "download" >>= toUrl)
        <*> (o .: "download_location" >>= toUrl)

data Urls = Urls
    { raw     :: !(Url Https)
    , full    :: !(Url Https)
    , regular :: !(Url Https)
    , small   :: !(Url Https)
    , thumb   :: !(Url Https)
    } deriving Show

instance Ae.FromJSON Urls where
    parseJSON = Ae.withObject "Urls" $ \o -> Urls
        <$> (o .: "raw" >>= toUrl)
        <*> (o .: "full" >>= toUrl)
        <*> (o .: "regular" >>= toUrl)
        <*> (o .: "small" >>= toUrl)
        <*> (o .: "thumb" >>= toUrl)

toUrl :: Text -> Ae.Parser (Url Https)
toUrl t = maybe mzero pure $ fmap fst $ useHttpsURI =<< URI.mkURI t
