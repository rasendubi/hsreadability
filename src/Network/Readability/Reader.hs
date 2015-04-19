{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ) where

import Control.Applicative ((<$>), (<*>))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Conduit (httpLbs, parseUrl, responseBody, urlEncodedBody, withManager)
import Network.HTTP.Types (parseQuery)

import qualified Web.Authenticate.OAuth as OAuth (newOAuth)
import Web.Authenticate.OAuth (OAuth, Credential, emptyCredential, newCredential,
        oauthServerName, oauthAuthorizeUri, oauthConsumerKey, oauthConsumerSecret,
        oauthRequestUri, oauthAccessTokenUri, signOAuth)

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

