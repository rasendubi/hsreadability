{-# LANGUAGE TemplateHaskell #-}
module Network.Readability.Parser
    ( ReadabilityParserToken(..)
    , Article(..)
    , parseByUrl
    ) where

import Data.Text (Text)
import Data.Aeson (decode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions)

import Network.URI (escapeURIString, isReserved)
import Network.HTTP.Conduit (simpleHttp)

newtype ReadabilityParserToken = ReadabilityParserToken String deriving (Eq, Show)

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

urlEscape :: String -> String
urlEscape = escapeURIString (not . isReserved)

parseByUrl :: ReadabilityParserToken -> String -> Maybe Int -> IO (Maybe Article)
parseByUrl (ReadabilityParserToken token) article_url maximumPages = fmap decode $ simpleHttp request
    where
        request = apiBase ++ "/parser?token=" ++ token ++ "&url=" ++ urlEscape article_url ++
            (maybe "" (("&max_pages=" ++) . show) maximumPages)
