{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Readability.Parser
    ( ParserToken(..)
    , Article(..)
    , parseByUrl
    , parseById
    ) where

import qualified Data.ByteString.Char8 as BS

import Data.Text (Text)
import Data.Aeson (decode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions)

import Network.HTTP.Conduit (parseUrl, responseBody, setQueryString, withManager, httpLbs)

newtype ParserToken = ParserToken BS.ByteString deriving (Eq, Show)

data Article = Article
    { content :: Text -- Html
    , domain :: Text -- Url
    , author :: Text
    , url :: Text -- Url
    , short_url :: Text -- Url
    , title :: Text
    , excerpt :: Text
    , direction :: Text -- make data type?
    , word_count :: Integer
    , total_pages :: Integer
    , date_published :: Maybe Text -- Date
    , dek :: Maybe Text -- or Html?
    , lead_image_url :: Maybe Text -- Url
    , next_page_id :: Maybe Int -- pageId?
    , rendered_pages :: Int
    } deriving (Show, Eq)

$(deriveFromJSON defaultOptions ''Article)

apiBase :: String
apiBase = "https://readability.com/api/content/v1"

parseByUrl :: ParserToken -> BS.ByteString -> Maybe Int -> IO (Maybe Article)
parseByUrl (ParserToken token) article_url maximumPages = do
    request <- fmap (setQueryString params) $ parseUrl (apiBase ++ "/parser")
    response <- withManager $ httpLbs request
    return $ decode $ responseBody response
    where
        params =
            [ ("token", Just token)
            , ("url", Just article_url)
            ] ++ maybeShowParam "max_pages" maximumPages

parseById :: ParserToken -> BS.ByteString -> Maybe Int -> IO (Maybe Article)
parseById (ParserToken token) article_id maximumPages = do
    request <- fmap (setQueryString params) $ parseUrl (apiBase ++ "/parser")
    response <- withManager $ httpLbs request
    return $ decode $ responseBody response
    where
        params =
            [ ("token", Just token)
            , ("id", Just article_id)
            ] ++ maybeShowParam "max_pages" maximumPages

maybeShowParam :: (Show a) => BS.ByteString -> Maybe a -> [(BS.ByteString, Maybe BS.ByteString)]
maybeShowParam name ma = maybe [] (\x -> [(name, Just $ BS.pack $ show x)]) ma
