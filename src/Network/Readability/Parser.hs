{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module is an interface to the Readability's Parser API.
--
-- To get more info, visit <https://www.readability.com/developers/api/parser>.
module Network.Readability.Parser
    ( ParserToken(..)

    -- * Parse article
    , Article(..)
    , parse
    , parseByUrl
    , parseById

    -- * Article status
    , ArticleStatus(..)
    , Status(..)
    , getContentStatus
    , getContentStatusByUrl
    , getContentStatusById

    -- * Confidence
    , Confidence(..)
    , getConfidence
    ) where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as BS

import Data.Text (Text)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Aeson.TH (deriveFromJSON, defaultOptions, fieldLabelModifier)

import Network.HTTP.Conduit (method, parseUrl, responseBody, responseHeaders, setQueryString, withManager, httpLbs)

-- | This is a Readability Parser API key.
--
-- You can get one at <https://www.readability.com/settings/account>.
newtype ParserToken = ParserToken BS.ByteString deriving (Eq, Show)

data Article = Article
    { content :: Maybe Text           -- ^ The main content of the article (in HTML)
    , domain :: Maybe Text            -- ^ Domain name of article host
    , author :: Maybe Text
    , url :: Text                     -- ^ URL of the article
    , short_url :: Maybe Text         -- ^ Shortened URL provided by Readability Shortener
    , title :: Maybe Text
    , excerpt :: Maybe Text
    , direction :: Maybe Text         -- ^ Text direction of article
    , word_count :: Integer
    , total_pages :: Maybe Integer    -- ^ Total number of pages in article
    , date_published :: Maybe Text
    , dek :: Maybe Text
    , lead_image_url :: Maybe Text
    , next_page_id :: Maybe Text
    , rendered_pages :: Maybe Int
    } deriving (Show, Eq)

$(deriveFromJSON defaultOptions ''Article)

-- | This type represent confidence with which Readability Parser recognizes content of article.
data Confidence = Confidence
    { conf_url :: Text           -- ^ Article URL
    , conf_confidence :: Double  -- ^ Parser's confidence
    } deriving (Show, Eq)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("conf_" :: String)) } ''Confidence)

-- | Represents article status
data ArticleStatus = ArticleStatus
    { as_article_id :: BS.ByteString  -- ^ Article id
    , as_article_status :: Status     -- ^ Article status
    } deriving (Show, Eq)

data Status
    = Invalid           -- ^ Unable to parse this URL for some reason
    | Unretrieved       -- ^ Readability knows of this article but hasn't yet retrieved its content,
                        --   or the cache expired
    | ProvidedByUser    -- ^ The content of this URL has been retrieved from at least one user
    | ValidatedByUsers  -- ^ The content was retrieved from multiple users and is validated
    | Fetched           -- ^ The content of this URL was fetched manually, and it has been cached
    deriving (Show, Eq)

apiPrefix :: String
apiPrefix = "https://readability.com/api/content/v1"

-- | Parses content of the article by URL.
--
-- This is a shortcut for 'parse'.
parseByUrl :: ParserToken -> BS.ByteString -> Maybe Int -> IO (Either String Article)
parseByUrl token articleUrl maximumPages = parse token (Just articleUrl) Nothing maximumPages

-- | Parses content of the article by article id.
--
-- This is a shortcut for 'parse'.
parseById :: ParserToken -> BS.ByteString -> Maybe Int -> IO (Either String Article)
parseById token articleId maximumPages = parse token Nothing (Just articleId) maximumPages

-- | Parses content of the given article and returns its description.
--
-- This is a @GET@ request to @/parser@ endpoint.
parse :: ParserToken         -- ^ Your Parser API token
      -> Maybe BS.ByteString -- ^ The article URL
      -> Maybe BS.ByteString -- ^ The article id
      -> Maybe Int           -- ^ The maximum number of pages to parse
      -> IO (Either String Article)
parse (ParserToken token) articleUrl articleId maximumPages = readabilityRequest "/parser" params
    where
        params =
            [ ("token", Just token)
            ]
            ++ maybeParam "url" articleUrl
            ++ maybeParam "id" articleId
            ++ maybeShowParam "max_pages" maximumPages

-- | Gets article's status by URL.
--
-- This functions is a shortcut for 'getContentStatus'.
getContentStatusByUrl :: ParserToken -> BS.ByteString -> IO (Maybe ArticleStatus)
getContentStatusByUrl token articleUrl = getContentStatus token (Just articleUrl) Nothing

-- | Gets article's status by id.
--
-- This functions is a shortcut for 'getContentStatus'.
getContentStatusById :: ParserToken -> BS.ByteString -> IO (Maybe ArticleStatus)
getContentStatusById token articleId = getContentStatus token Nothing (Just articleId)

-- | Gets article's status.
--
-- This is a @HEAD@ request to @/parser@ endpoint.
getContentStatus :: ParserToken               -- ^ Your Parser API key
                 -> Maybe BS.ByteString       -- ^ Article URL
                 -> Maybe BS.ByteString       -- ^ Article id
                 -> IO (Maybe ArticleStatus)
getContentStatus (ParserToken token) articleUrl articleId = contentStatusRequest params
    where
        params =
            [ ("token", Just token)
            ]
            ++ maybeParam "url" articleUrl
            ++ maybeParam "id" articleId

contentStatusRequest :: [(BS.ByteString, Maybe BS.ByteString)] -> IO (Maybe ArticleStatus)
contentStatusRequest params = do
    query <- setQueryString params <$> parseUrl (apiPrefix ++ "/parser")
    response <- withManager $ httpLbs query{ method = "HEAD" }
    let headers = responseHeaders response
    return $ do
        article_id <- lookup "X-Article-Id" headers
        article_status <- parseStatus =<< lookup "X-Article-Status" headers
        return $ ArticleStatus article_id article_status

-- | Gets parser's confidence by URL.
--
-- This a @GET@ request to @/confidence@
getConfidence :: BS.ByteString          -- ^ Article URL
              -> IO (Either String Confidence)
getConfidence article_url = readabilityRequest "/confidence" [ ("url", Just article_url) ]

readabilityRequest :: FromJSON a => String -> [(BS.ByteString, Maybe BS.ByteString)] -> IO (Either String a)
readabilityRequest api params = do
    request <- setQueryString params <$> parseUrl (apiPrefix ++ api)
    response <- withManager $ httpLbs request
    return $ eitherDecode $ responseBody response

maybeShowParam :: (Show a) => BS.ByteString -> Maybe a -> [(BS.ByteString, Maybe BS.ByteString)]
maybeShowParam name = maybeParam name . Just . BS.pack . show

maybeParam :: BS.ByteString -> Maybe BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)]
maybeParam name = maybe [] $ \x -> [(name, Just x)]

parseStatus :: BS.ByteString -> Maybe Status
parseStatus "INVALID" = Just Invalid
parseStatus "UNRETRIEVED" = Just Unretrieved
parseStatus "PROVIDED_BY_USER" = Just ProvidedByUser
parseStatus "VALIDATED_BY_USERS" = Just ValidatedByUsers
parseStatus "FETCHED" = Just Fetched
parseStatus _ = Nothing
