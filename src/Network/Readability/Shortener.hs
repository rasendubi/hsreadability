{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Readability.Shortener
    ( Meta(..)
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


data Meta = Meta
    { meta_article :: Maybe Article
    , meta_url :: Maybe Text -- Url
    , meta_rdd_url :: Text -- Url
    , meta_id :: Text
    , meta_full_url :: Maybe Text
    } deriving (Show)

data ShortenerResponse = ShortenerResponse
    { meta :: Meta
    , messages :: [Text]
    , success :: Bool
    } deriving (Show)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("meta_" :: String)) } ''Meta)
$(deriveFromJSON defaultOptions ''ShortenerResponse)

apiPrefix :: String
apiPrefix = "https://readability.com/api/shortener/v1/urls"

shortenUrl :: String -> IO (Either String ShortenerResponse)
shortenUrl source_url  = shortenUrlRequest $ BS.pack source_url

shortenUrlRequest :: BS.ByteString -> IO (Either String ShortenerResponse)
shortenUrlRequest source_url = do
    initRequest <- parseUrl apiPrefix
    let request = urlEncodedBody [("url", source_url)] initRequest
    response <- withManager $ httpLbs request
    return $ eitherDecode $ responseBody response


retrieveUrl :: String -> IO (Either String ShortenerResponse)
retrieveUrl = retrieveUrlRequest

retrieveUrlRequest :: String -> IO (Either String ShortenerResponse)
retrieveUrlRequest url_id = do
    request <- parseUrl (apiPrefix ++ "/" ++ url_id)
    response <- withManager $ httpLbs request
    return $ eitherDecode $ responseBody response
