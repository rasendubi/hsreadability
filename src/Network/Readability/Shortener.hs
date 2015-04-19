{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module is an interface to the Readability's Shortener API.
--
-- To get more info, visit <https://www.readability.com/developers/api/shortener>.
module Network.Readability.Shortener
    ( Article(..)

      -- * Shorten and retrieve URL
    , ShortenerMeta(..)
    , ShortenerResponse(..)
    , shortenUrl
    , retrieveUrl
    ) where

import qualified Data.ByteString.Char8 as BS

import Data.Text (Text)
import Data.Aeson (eitherDecode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions, fieldLabelModifier)

import Network.HTTP.Conduit (parseUrl, responseBody, withManager, httpLbs, urlEncodedBody)

import Network.Readability.Parser (Article(..))

-- | Contains main data
data ShortenerMeta = ShortenerMeta
    { smeta_article :: Maybe Article
    , smeta_url :: Maybe Text
    , smeta_rdd_url :: Text         -- ^ The shortened URL
    , smeta_id :: Text              -- ^ The id of shortened URL
    , smeta_full_url :: Maybe Text  -- ^ The Article URL
    } deriving (Show)

-- | Response from Shortener API
data ShortenerResponse = ShortenerResponse
    { se_meta :: ShortenerMeta
    , se_messages :: [Text]         -- ^ The response messages
    , se_success :: Bool
    } deriving (Show)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("smeta_" :: String)) } ''ShortenerMeta)
$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("se_" :: String)) } ''ShortenerResponse)

apiPrefix :: String
apiPrefix = "https://readability.com/api/shortener/v1/urls"

-- | Create a new shortened URL
shortenUrl :: String -> IO (Either String ShortenerResponse)
shortenUrl source_url  = shortenUrlRequest $ BS.pack source_url

shortenUrlRequest :: BS.ByteString -> IO (Either String ShortenerResponse)
shortenUrlRequest source_url = do
    initRequest <- parseUrl apiPrefix
    let request = urlEncodedBody [("url", source_url)] initRequest
    response <- withManager $ httpLbs request
    return $ eitherDecode $ responseBody response

-- | Retrieve a single shortened URL
retrieveUrl :: String -> IO (Either String ShortenerResponse)
retrieveUrl = retrieveUrlRequest

retrieveUrlRequest :: String -> IO (Either String ShortenerResponse)
retrieveUrlRequest url_id = do
    request <- parseUrl (apiPrefix ++ "/" ++ url_id)
    response <- withManager $ httpLbs request
    return $ eitherDecode $ responseBody response
