-- |
-- Module: Network.Payments.PayPal.Auth
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Auth
( ClientID
, Secret
, Seconds
, AccessToken(..)
, AccessTokenWithExpiration
, AccessTokenError(..)
, AccessTokenResult
, fetchAccessToken
, fetchAccessTokenWithExpiration
, safeExpirationTime
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time.Clock
import qualified Network.HTTP.Client as HTTP
import Network.Wreq
import qualified Network.Wreq.Types as WTypes
import Network.Payments.PayPal.Environment

-- |PayPal client ID with which to execute actions.
type ClientID = String

-- |PayPal secret of user with which to execute actions.
type Secret = String

-- |Number representing seconds of time.
type Seconds = Integer

-- |Access token returned from OAuth.
data AccessToken = AccessToken
  { aTokenScope :: [String]
  , aToken :: String
  , aTokenType  :: String
  , aTokenAppId :: String
  , aTokenExpires :: Seconds
  } deriving (Eq, Show)

-- |An access token from OAuth together with its UTC expiration time.
type AccessTokenWithExpiration = (AccessToken, UTCTime)

-- |Error while fetching access token.
data AccessTokenError = AccessTokenHttpError HTTP.HttpException |
                        AccessTokenStatusError |
                        -- Contains error message and JSON text.
                        AccessTokenParseError String LBS.ByteString

-- |Either an access token or the error encountered while fetching it.
type AccessTokenResult = Either AccessTokenError AccessToken

-- |Either an access token with an expiration or the error encountered while
-- fetching it
type AccessTokenWithExpirationResult =
  Either AccessTokenError AccessTokenWithExpiration

instance FromJSON AccessToken where
  parseJSON (Object obj) =
    AccessToken <$>
    (map T.unpack <$> T.split (== ' ') <$> (obj .: "scope")) <*>
    obj .: "access_token" <*>
    obj .: "token_type" <*>
    obj .: "app_id" <*>
    obj .: "expires_in"
  parseJSON _ = mzero

-- |Use a PayPal environment and login credentials to get an OAuth access token.
fetchAccessToken :: EnvironmentUrl -> ClientID -> Secret -> IO AccessTokenResult
fetchAccessToken (EnvironmentUrl url) username password = do
  let usernameBS = BS8.pack username
      passwordBS = BS8.pack password
      fullUrl = url ++ "/v1/oauth2/token"
      options' = defaults & header "Accept" .~ ["application/json"] &
                            auth ?~ basicAuth usernameBS passwordBS
      contentType = "application/x-www-form-urlencoded"
      content = "grant_type=client_credentials"
      payload = WTypes.Raw contentType $ HTTP.RequestBodyBS content
  responseOrErr <- (try $ postWith options' fullUrl payload) ::
                   IO (Either HTTP.HttpException (Response LBS.ByteString))
  case responseOrErr of
    Left err -> return $ Left $ AccessTokenHttpError err
    Right response ->
      if response ^. responseStatus . statusCode == 200 then
        let responseText = response ^. responseBody
        in return $ case eitherDecode responseText of
             Left errMsg -> Left $ AccessTokenParseError errMsg responseText
             Right result -> Right result
      else
        return $ Left AccessTokenStatusError

-- |Use a PayPal environment and login credentials to get an OAuth access token
-- with an expiration time.
fetchAccessTokenWithExpiration :: EnvironmentUrl -> ClientID -> Secret ->
                                  IO AccessTokenWithExpirationResult
fetchAccessTokenWithExpiration environment username password= do
  currentTime <- getCurrentTime
  accessTokenOrErr <- fetchAccessToken environment username password
  let getExpire accToken = (accToken, safeExpirationTime currentTime accToken)
  return $ either Left (Right . getExpire) accessTokenOrErr

-- |Time at which the token should be considered expired. This is a few seconds
-- before the time that PayPal gives us. Parameters are the time at which the
-- access token was retrieved and the access token.
safeExpirationTime :: UTCTime -> AccessToken -> UTCTime
safeExpirationTime currentTime token =
  let safetyBuffer = 10 -- Seconds.
      seconds = aTokenExpires token - safetyBuffer
  in addUTCTime (fromIntegral seconds) currentTime
