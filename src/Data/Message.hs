module Data.Message
    ( InMessage (..)
    , GMIUrl (..)
    , OutMessage (..)
    ) where

import RIO

import Data.Aeson ((.:), (.=))
import Network.HTTP.Req
    ( Url
    , Scheme (Https)
    , useHttpsURI
    , renderUrl
    )

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import qualified RIO.Vector as V
import qualified Text.URI as URI

data InMessage = InMessage
    { hasAttach :: !Bool
    , groupId   :: !Word
    , isUser    :: !Bool
    , text      :: !Text
    } deriving Show

instance Ae.FromJSON InMessage where
    parseJSON = Ae.withObject "InMessage" $ \o -> InMessage
        <$> (o .: "attachments" >>= Ae.withArray "attachments" (pure . not . null))
        <*> o .: "group-id"
        <*> ((==) ("user" :: Text) <$> o .: "sender_type")
        <*> o .: "text"


newtype GMIUrl = GMIUrl
    { unGMIUrl :: Url Https
    } deriving Show

instance Ae.FromJSON GMIUrl where
    parseJSON = Ae.withObject "payload" $ \o -> do
        p <- o .: "payload"
        u <- p .: "url"
        GMIUrl <$> toUrl u
      where
        toUrl :: Text -> Ae.Parser (Url Https)
        toUrl t = maybe mzero pure $ fmap fst $ useHttpsURI =<< URI.mkURI t

data OutMessage = OutMessage
    { image :: !GMIUrl
    } deriving Show

instance Ae.ToJSON OutMessage where
    toJSON om = Ae.object
        [ "attachments" .=
            ( Ae.Array $ V.singleton attach )
        -- , "bot_id" .= botId
        ]
      where
        attach :: Ae.Value
        attach = Ae.object
            [ "type" .= ("image" :: Text)
            , "url"  .= url
            ]
        url :: Text
        url = renderUrl $ unGMIUrl $ image om

  -- {
  -- "bot_id"  : "j5abcdefg",
  -- "text"    : "Hello world",
  -- "attachments" : [
  --   {
  --     "type"  : "image",
  --     "url"   : "https://i.groupme.com/somethingsomething.large"
  --   }
  -- ]
-- }
