{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Network.Readability.Reader
    (
    -- * Authentication
    -- ** OAuth
      OAuth
    , newOAuth
    , oauthConsumerKey
    , oauthConsumerSecret
    , xauth

    -- ** OAuth Endoints
    , oauthAuthorizeEndpoint
    , oauthRequestTokenEndpoint
    , oauthAccessTokenEndpoint

    -- * Article
    , getArticle

    -- * Bookmarks
    -- ** Types
    , BookmarksFilters(..)
    , defaultBookmarksFilters
    , BookmarksResponse(..)
    , BookmarksConditions(..)
    , BookmarksMeta(..)
    , Bookmark(..)
    , Article(..)
    , TagsResponse(..)
    , Tag(..)
    , BookmarkLocation(..)
    , Order(..)

    -- ** Requests
    , getBookmarks
    , addBookmark
    , getBookmark
    , updateBookmark
    , deleteBookmark
    , getBookmarkTags
    , addBookmarkTags
    , deleteBookmarkTag
    ) where

import Control.Applicative ((<$>), (<*>))

import Data.Aeson (FromJSON(..), Value(String), eitherDecode)
import Data.Aeson.TH (defaultOptions, deriveFromJSON, fieldLabelModifier)
import Data.Maybe (catMaybes)
import Data.Default (Default(def))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Conduit (Response, httpLbs, parseUrl, responseBody, responseHeaders,
        setQueryString, urlEncodedBody, withManager, method)
import Network.HTTP.Types (parseQuery)

import Text.XML.XSD.DateTime (DateTime)

import qualified Web.Authenticate.OAuth as OAuth (newOAuth)
import Web.Authenticate.OAuth (OAuth, Credential, emptyCredential, newCredential,
        oauthServerName, oauthAuthorizeUri, oauthConsumerKey, oauthConsumerSecret,
        oauthRequestUri, oauthAccessTokenUri, signOAuth)

import Network.Readability.Parser (Article(..))

apiPrefix :: String
apiPrefix = "https://readability.com/api/rest/v1"

-- | OAuth endpoint
oauthAuthorizeEndpoint, oauthRequestTokenEndpoint, oauthAccessTokenEndpoint :: String
oauthAuthorizeEndpoint    = apiPrefix ++ "/oauth/authorize/"
oauthRequestTokenEndpoint = apiPrefix ++ "/oauth/request_token/"
oauthAccessTokenEndpoint  = apiPrefix ++ "/oauth/access_token/"

-- | Default value for OAuth datatype. You must specify at least consumer key and secret.
--
-- Example usage:
--
-- @
-- myAuth = newOAuth
--     { oauthConsumerKey = "consumer key"
--     , oauthConsumerSecret = "consumer secret"
--     }
-- @
newOAuth :: OAuth
newOAuth = OAuth.newOAuth
    { oauthServerName = "www.readability.com"
    , oauthAuthorizeUri = oauthAuthorizeEndpoint
    , oauthRequestUri = oauthRequestTokenEndpoint
    , oauthAccessTokenUri = oauthAccessTokenEndpoint
    }

-- | XAuth is used to receive OAuth token using owner's username and password directly.
--
-- In most cases, you should do your best to support OAuth proper.
xauth :: OAuth                 -- ^ OAuth client
      -> BS.ByteString         -- ^ Owner username
      -> BS.ByteString         -- ^ Owner password
      -> IO (Maybe Credential) -- ^ Token and secret
xauth oauth username password = do
    r <- parseUrl (oauthAccessTokenUri oauth)
    let bodyParams =
            [ ("x_auth_username", username)
            , ("x_auth_password", password)
            , ("x_auth_method", "client_auth")
            ]
    let request = urlEncodedBody bodyParams r
    signedRequest <- signOAuth oauth emptyCredential request
    response <- withManager $ httpLbs signedRequest
    let res = parseQuery . BL.toStrict . responseBody $ response
    return $ do
        token <- lookup "oauth_token" res
        secret <- lookup "oauth_token_secret" res
        newCredential <$> token <*> secret

-- | Gets article by id.
--
-- This is a @GET@ request to @/articles/@ endpoint.
getArticle :: OAuth         -- ^ Client OAuth
           -> Credential    -- ^ Access token and secret
           -> BS.ByteString -- ^ Article id
           -> IO (Either String Article)
getArticle oauth cred articleId = do
    req <- parseUrl (apiPrefix ++ "/articles/" ++ BS.unpack articleId)
    signedReq <- signOAuth oauth cred req
    response <- withManager $ httpLbs signedReq
    return $ eitherDecode $ responseBody response

data BookmarksFilters = BookmarksFilters
    { bfArchive :: Maybe Bool -- ^ Archieved status
    , bfFavorite :: Maybe Bool
    , bfDomain :: Maybe String
    , bfAddedSince :: Maybe DateTime
    , bfAddedUntil :: Maybe DateTime
    , bfOpenedSince :: Maybe DateTime
    , bfOpenedUntil :: Maybe DateTime
    , bfArchivedSince :: Maybe DateTime
    , bfArchivedUntil :: Maybe DateTime
    , bfFavoritedSince :: Maybe DateTime
    , bfFavoritedUntil :: Maybe DateTime
    , bfUpdatedSince :: Maybe DateTime
    , bfUpdatedUntil :: Maybe DateTime
    } deriving (Show, Eq)

instance Default BookmarksFilters where
    def = BookmarksFilters
        { bfArchive = def
        , bfFavorite = def
        , bfDomain = def
        , bfAddedSince = def
        , bfAddedUntil = def
        , bfOpenedSince = def
        , bfOpenedUntil = def
        , bfArchivedSince = def
        , bfArchivedUntil = def
        , bfFavoritedSince = def
        , bfFavoritedUntil = def
        , bfUpdatedSince = def
        , bfUpdatedUntil = def
        }

defaultBookmarksFilters :: BookmarksFilters
defaultBookmarksFilters = def

bookmarkFiltersToParams :: BookmarksFilters -> [(BS.ByteString, Maybe BS.ByteString)]
bookmarkFiltersToParams BookmarksFilters{..} = catMaybes
    [ boolParam   "archive"         bfArchive
    , boolParam   "favorite"        bfFavorite
    , stringParam "domain"          bfDomain
    , param       "added_since"     bfAddedSince
    , param       "added_until"     bfAddedUntil
    , param       "opened_since"    bfOpenedSince
    , param       "opened_until"    bfOpenedUntil
    , param       "archived_since"  bfArchivedSince
    , param       "archived_until"  bfArchivedUntil
    , param       "favorited_since" bfFavoritedSince
    , param       "favorited_until" bfFavoritedUntil
    , param       "updated_since"   bfUpdatedSince
    , param       "updated_until"   bfUpdatedUntil
    ]

boolParam :: BS.ByteString -> Maybe Bool -> Maybe (BS.ByteString, Maybe BS.ByteString)
boolParam name = fmap $ (name,) . Just . bshow . fromEnum

boolParam' :: BS.ByteString -> Maybe Bool -> Maybe (BS.ByteString, BS.ByteString)
boolParam' name = fmap $ (name,) . bshow . fromEnum

stringParam :: BS.ByteString -> Maybe String -> Maybe (BS.ByteString, Maybe BS.ByteString)
stringParam name = fmap $ (name,) . Just . BS.pack

param :: (Show a) => BS.ByteString -> Maybe a -> Maybe (BS.ByteString, Maybe BS.ByteString)
param name = fmap $ (name,) . Just . bshow

param' :: (Show a) => BS.ByteString -> Maybe a -> Maybe (BS.ByteString, BS.ByteString)
param' name = fmap $ (name,) . bshow

stringListParam :: BS.ByteString -> [String] -> Maybe (BS.ByteString, Maybe BS.ByteString)
stringListParam _ [] = Nothing
stringListParam name xs = Just . (name,) . Just . BS.intercalate "," $ fmap BS.pack xs

bshow :: Show a => a -> BS.ByteString
bshow = BS.pack . show

data Order = DateAddedAsc | DateAddedDesc | DateUpdatedAsc | DateUpdatedDesc
    deriving (Eq)

instance Show Order where
    show DateAddedAsc = "date_added"
    show DateAddedDesc = "-date_added"
    show DateUpdatedAsc = "date_updated"
    show DateUpdatedDesc = "-date_updated"

instance FromJSON Order where
    parseJSON (String "date_added") = return DateAddedAsc
    parseJSON (String "-date_added") = return DateAddedDesc
    parseJSON (String "date_updated") = return DateUpdatedAsc
    parseJSON (String "-date_updated") = return DateUpdatedDesc
    parseJSON _ = fail "Cant't parse Order"

data BookmarksConditions = BookmarksConditions
    { bc_archive :: Maybe Int
    , bc_favorite :: Maybe Int
    , bc_domain :: Maybe Text
    , bc_added_since :: Maybe Text
    , bc_added_until :: Maybe Text
    , bc_opened_since :: Maybe Text
    , bc_opened_until :: Maybe Text
    , bc_archived_since :: Maybe Text
    , bc_archived_until :: Maybe Text
    , bc_favorited_since :: Maybe Text
    , bc_favorited_until :: Maybe Text
    , bc_updated_since :: Maybe Text
    , bc_updated_until :: Maybe Text
    , bc_order :: Maybe Order
    , bc_user :: Maybe Text
    , bc_page :: Maybe Integer
    , bc_per_page :: Maybe Integer
    , bc_exclude_accessibility :: Maybe Text
    } deriving (Eq, Show)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("bc_" :: String)) } ''BookmarksConditions)

data BookmarksMeta = BookmarksMeta
    { bmeta_num_pages :: Maybe Integer
    , bmeta_page :: Maybe Integer
    , bmeta_item_count_total :: Maybe Integer
    , bmeta_item_count :: Maybe Integer
    } deriving (Eq, Show)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("bmeta_" :: String)) } ''BookmarksMeta)

data Tag = Tag
    { t_text :: Text
    , t_id :: Integer
    , t_applied_count :: Maybe Integer
    } deriving (Eq, Show)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("t_" :: String)) } ''Tag)

data Bookmark = Bookmark
    { b_user_id :: Integer
    , b_read_percent :: Text
    , b_date_updated :: Text
    , b_favorite :: Bool
    , b_archive :: Bool
    , b_article :: Article
    , b_id :: Integer
    , b_date_archived :: Maybe Text
    , b_date_opened :: Maybe Text
    , b_date_added :: Maybe Text
    , b_date_favorited :: Maybe Text
    , b_article_href :: Text
    , b_tags :: [Tag]
    } deriving (Eq, Show)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("b_" :: String)) } ''Bookmark)

data BookmarksResponse = BookmarksResponse
    { br_conditions :: BookmarksConditions
    , br_meta :: BookmarksMeta
    , br_bookmarks :: [Bookmark]
    } deriving (Eq, Show)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("br_" :: String)) } ''BookmarksResponse)

getBookmarks :: OAuth
             -> Credential
             -> BookmarksFilters
             -> Maybe Order
             -> Maybe Integer -- ^ Page
             -> Maybe Integer -- ^ Per page
             -> Maybe Bool -- ^ Only deleted
             -> [String] -- ^ Tags
             -> IO (Either String BookmarksResponse)
getBookmarks oauth credential bfilter mOrder mPage mPerPage mOnlyDeleted tags = do
    let params = bookmarkFiltersToParams bfilter ++ catMaybes
            [ param "order" mOrder
            , param "page" mPage
            , param "per_page" mPerPage
            , boolParam "only_deleted" mOnlyDeleted
            , stringListParam "tags" tags
            ]
    r <- parseUrl (apiPrefix ++ "/bookmarks")
    signedRequest <- signOAuth oauth credential $ setQueryString params r
    response <- withManager $ httpLbs signedRequest
    return $ eitherDecode $ responseBody response

data BookmarkLocation = BookmarkLocation
    { blLocation :: Maybe BS.ByteString
    , blArticleLocation :: Maybe BS.ByteString
    } deriving (Eq, Show)

-- | Add bookmark.
--
-- This is a @POST@ request to @/bookmarks@.
addBookmark :: OAuth
            -> Credential
            -> BS.ByteString -- ^ Article URL
            -> Maybe Bool    -- ^ Favorite
            -> Maybe Bool    -- ^ Archive
            -> Maybe Bool    -- ^ Allow duplicates
            -> IO BookmarkLocation
addBookmark oauth cred articleUrl mFavorite mArchive mAllowDuplicates = do
    let params = catMaybes
            [ Just ("url", articleUrl)
            , boolParam' "favorite" mFavorite
            , boolParam' "archive" mArchive
            , boolParam' "allow_duplicates" mAllowDuplicates
            ]
    req <- parseUrl (apiPrefix ++ "/bookmarks")
    signedRequest <- signOAuth oauth cred $ urlEncodedBody params req
    response <- withManager $ httpLbs signedRequest
    let headers = responseHeaders response
    return $ BookmarkLocation (lookup "Location" headers) (lookup "X-Article-Location" headers)

-- | Get bookmark by id.
--
-- This is a @GET@ request to @/bookmarks/{bookmarkId}@.
getBookmark :: OAuth
            -> Credential
            -> Integer    -- ^ Bookmark id
            -> IO (Either String Bookmark)
getBookmark oauth cred bookmarkId = do
    req <- parseUrl (apiPrefix ++ "/bookmarks/" ++ show bookmarkId)
    signedRequest <- signOAuth oauth cred req
    response <- withManager $ httpLbs signedRequest
    return $ eitherDecode $ responseBody response

-- | Update bookmark.
--
-- This a @POST@ request to @/bookmarks/{bookmarkId}@.
updateBookmark :: OAuth
               -> Credential
               -> Integer      -- ^ Bookmark id
               -> Maybe Bool   -- ^ Favorite
               -> Maybe Bool   -- ^ Archive
               -> Maybe Double -- ^ Read percent
               -> IO (Either String Bookmark)
updateBookmark oauth cred bookmarkId mFavorite mArchive mReadPercent = do
    let params = catMaybes
            [ boolParam' "favorite" mFavorite
            , boolParam' "archive" mArchive
            , param' "read_percent" mReadPercent
            ]
    req <- parseUrl (apiPrefix ++ "/bookmarks/" ++ show bookmarkId)
    signedRequest <- signOAuth oauth cred $ urlEncodedBody params req
    response <- withManager $ httpLbs signedRequest
    return $ eitherDecode $ responseBody response

-- | Delete bookmark by id.
--
-- This is a @DELETE@ request to @/bookmarks/{bookmarkId}@.
deleteBookmark :: OAuth
               -> Credential
               -> Integer    -- ^ Bookmark id
               -> IO (Response BL.ByteString)
deleteBookmark oauth cred bookmarkId = do
    req <- parseUrl $ apiPrefix ++ "/bookmarks/" ++ show bookmarkId
    signedRequest <- signOAuth oauth cred $ req{ method = "DELETE" }
    withManager $ httpLbs signedRequest

data TagsResponse = TagsResponse { tr_tags :: [Tag] } deriving (Eq, Show)

$(deriveFromJSON defaultOptions{ fieldLabelModifier = drop (length ("tr_" :: String)) } ''TagsResponse)

-- | Get bookmark tags
--
-- This is a @GET@ request to @/bookmarks/{bookmarkId}/tags@.
getBookmarkTags :: OAuth
                -> Credential
                -> Integer    -- ^ Bookmark id
                -> IO (Either String TagsResponse)
getBookmarkTags oauth cred bookmarkId = do
    req <- parseUrl $ apiPrefix ++ "/bookmarks/" ++ show bookmarkId ++ "/tags"
    signedRequest <- signOAuth oauth cred req
    response <- withManager $ httpLbs signedRequest
    return $ eitherDecode $ responseBody response

-- | Add tags to bookmark.
--
-- This is a @POST@ request to @/bookmarks/{bookmarkId}/tags@.
addBookmarkTags :: OAuth
                -> Credential
                -> Integer    -- ^ Bookmark id
                -> [Text]   -- ^ Tags
                -> IO (Either String TagsResponse)
addBookmarkTags oauth cred bookmarkId tags = do
    let params = [ ("tags", T.encodeUtf8 $ T.intercalate "," tags) ]
    req <- parseUrl $ apiPrefix ++ "/bookmarks/" ++ show bookmarkId ++ "/tags"
    signedRequest <- signOAuth oauth cred $ urlEncodedBody params req
    response <- withManager $ httpLbs signedRequest
    return $ eitherDecode $ responseBody response

-- | Delete given tag from bookmark.
--
-- This is a @DELETE@ request to @/bookmark/{bookmarkId}/tags/{tagId}@.
deleteBookmarkTag :: OAuth
                  -> Credential
                  -> Integer    -- ^ Bookmark id
                  -> Integer    -- ^ Tag id
                  -> IO (Response BL.ByteString)
deleteBookmarkTag oauth cred bookmarkId tagId = do
    req <- parseUrl $ apiPrefix ++ "/bookmarks/" ++ show bookmarkId ++ "/tags/" ++ show tagId
    signedRequest <- signOAuth oauth cred $ req{ method = "DELETE" }
    withManager $ httpLbs signedRequest
