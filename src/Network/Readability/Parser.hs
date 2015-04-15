{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Readability.Parser
    ( ParserToken(..)
    , Article(..)
    , Confidence(..)
    , ArticleStatus(..)
    , Status(..)
    , parseByUrl
    , parseById
    , getContentStatusByUrl
    , getContentStatusById
    , getConfidence
    ) where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as BS

import Data.Text (Text)
import Data.Aeson (FromJSON, decode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions, fieldLabelModifier)

import Network.HTTP.Conduit (method, parseUrl, responseBody, responseHeaders, setQueryString, withManager, httpLbs)

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

data ArticleStatus = ArticleStatus
    { as_article_id :: BS.ByteString
    , as_article_status :: Status
    } deriving (Show, Eq)

data Status = Invalid | Unretrieved | ProvidedByUser | ValidatedByUsers | Fetched
    deriving (Show, Eq)

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

getContentStatusByUrl :: ParserToken -> BS.ByteString -> IO (Maybe ArticleStatus)
getContentStatusByUrl (ParserToken token) article_url = contentStatusRequest params
    where
        params =
            [ ("token", Just token)
            , ("url", Just article_url)
            ]

getContentStatusById :: ParserToken -> BS.ByteString -> IO (Maybe ArticleStatus)
getContentStatusById (ParserToken token) article_id = contentStatusRequest params
    where
        params =
            [ ("token", Just token)
            , ("id", Just article_id)
            ]

contentStatusRequest :: [(BS.ByteString, Maybe BS.ByteString)] -> IO (Maybe ArticleStatus)
contentStatusRequest params = do
    query <- setQueryString params <$> parseUrl (apiPrefix ++ "/parser")
    response <- withManager $ httpLbs query{ method = "HEAD" }
    let headers = responseHeaders response
    return $ do
        article_id <- lookup "X-Article-Id" headers
        article_status <- parseStatus =<< lookup "X-Article-Status" headers
        return $ ArticleStatus article_id article_status

getConfidence :: BS.ByteString -> IO (Maybe Confidence)
getConfidence article_url = readabilityRequest "/confidence" [ ("url", Just article_url) ]

readabilityRequest :: FromJSON a => String -> [(BS.ByteString, Maybe BS.ByteString)] -> IO (Maybe a)
readabilityRequest api params = do
    request <- setQueryString params <$> parseUrl (apiPrefix ++ api)
    response <- withManager $ httpLbs request
    return $ decode $ responseBody response

maybeShowParam :: (Show a) => BS.ByteString -> Maybe a -> [(BS.ByteString, Maybe BS.ByteString)]
maybeShowParam name = maybe [] $ \x -> [(name, Just $ BS.pack $ show x)]

parseStatus :: BS.ByteString -> Maybe Status
parseStatus "INVALID" = Just Invalid
parseStatus "UNRETRIEVED" = Just Unretrieved
parseStatus "PROVIDED_BY_USER" = Just ProvidedByUser
parseStatus "VALIDATED_BY_USERS" = Just ValidatedByUsers
parseStatus "FETCHED" = Just Fetched
parseStatus _ = Nothing
