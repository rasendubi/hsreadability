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
import Data.Aeson (decode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions, fieldLabelModifier)

import Network.HTTP.Conduit (RequestBody(..), method, parseUrl, responseBody, requestBody,
        requestHeaders, withManager, httpLbs)

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

shortenUrl :: String -> IO (Maybe ShortenerResponse)
shortenUrl source_url  = shortenUrlRequest $ BS.pack source_url

shortenUrlRequest :: BS.ByteString -> IO (Maybe ShortenerResponse)
shortenUrlRequest source_url = do
    initRequest <- parseUrl apiPrefix
    let request = initRequest { method = "POST"
                              , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                              , requestBody = RequestBodyBS (BS.concat [BS.pack "url=", source_url])
                              }
    response <- withManager $ httpLbs request
    return $ decode $ responseBody response


retrieveUrl :: String -> IO (Maybe ShortenerResponse)
retrieveUrl = retrieveUrlRequest

retrieveUrlRequest :: String -> IO (Maybe ShortenerResponse)
retrieveUrlRequest url_id = do
    request <- parseUrl (apiPrefix ++ "/" ++ url_id)
    response <- withManager $ httpLbs request
    return $ decode $ responseBody response
