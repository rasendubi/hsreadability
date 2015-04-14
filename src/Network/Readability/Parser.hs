{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Readability.Parser
    ( ParserToken(..)
    , Article(..)
    , Confidence(..)
    , parseByUrl
    , parseById
    , getConfidence
    ) where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as BS

import Data.Text (Text)
import Data.Aeson (FromJSON, decode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions, fieldLabelModifier)

import Network.HTTP.Conduit (parseUrl, responseBody, setQueryString, withManager, httpLbs)

newtype ParserToken = ParserToken BS.ByteString deriving (Eq, Show)

data Article = Article
    { content :: Maybe Text -- Html
    , domain :: Text -- Url
    , author :: Maybe Text
    , url :: Text -- Url
    , short_url :: Text -- Url
    , title :: Maybe Text
    , excerpt :: Maybe Text
    , direction :: Maybe Text -- make data type?
    , word_count :: Integer
    , total_pages :: Integer
    , date_published :: Maybe Text -- Date
    , dek :: Maybe Text -- or Html?
    , lead_image_url :: Maybe Text -- Url
    , next_page_id :: Maybe Text -- pageId?
    , rendered_pages :: Int
    } deriving (Show, Eq)

$(deriveFromJSON defaultOptions ''Article)

data Confidence = Confidence
    { conf_url :: Text
    , conf_confidence :: Double
    } deriving (Show, Eq)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("conf_" :: String)) } ''Confidence)

apiPrefix :: String
apiPrefix = "https://readability.com/api/content/v1"

parseByUrl :: ParserToken -> BS.ByteString -> Maybe Int -> IO (Maybe Article)
parseByUrl (ParserToken token) article_url maximumPages = readabilityRequest "/parser" params
    where
        params =
            [ ("token", Just token)
            , ("url", Just article_url)
            ] ++ maybeShowParam "max_pages" maximumPages

parseById :: ParserToken -> BS.ByteString -> Maybe Int -> IO (Maybe Article)
parseById (ParserToken token) article_id maximumPages = readabilityRequest "/parser" params
    where
        params =
            [ ("token", Just token)
            , ("id", Just article_id)
            ] ++ maybeShowParam "max_pages" maximumPages

getConfidence :: BS.ByteString -> IO (Maybe Confidence)
getConfidence article_url = readabilityRequest "/confidence" [ ("url", Just article_url) ]

readabilityRequest :: FromJSON a => String -> [(BS.ByteString, Maybe BS.ByteString)] -> IO (Maybe a)
readabilityRequest api params = do
    request <- setQueryString params <$> parseUrl (apiPrefix ++ api)
    response <- withManager $ httpLbs request
    return $ decode $ responseBody response

maybeShowParam :: (Show a) => BS.ByteString -> Maybe a -> [(BS.ByteString, Maybe BS.ByteString)]
maybeShowParam name = maybe [] $ \x -> [(name, Just $ BS.pack $ show x)]
