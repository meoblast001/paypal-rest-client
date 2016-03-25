-- |
-- Module: Network.Payments.PayPal.Auth
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Auth
( ClientID
, Secret
, Seconds
, AccessToken(..)
, AccessTokenWithExpiration
, fetchAccessToken
, fetchAccessTokenWithExpiration
, safeExpirationTime
) where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
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
  } deriving (Show)

-- |An access token from OAuth together with its UTC expiration time.
type AccessTokenWithExpiration = (AccessToken, UTCTime)

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
fetchAccessToken :: EnvironmentUrl -> ClientID -> Secret ->
                    IO (Maybe AccessToken)
fetchAccessToken (EnvironmentUrl url) username password = do
  let usernameBS = BS8.pack username
      passwordBS = BS8.pack password
      fullUrl = url ++ "/v1/oauth2/token"
      options' = defaults & header "Accept" .~ ["application/json"] &
                            auth ?~ basicAuth usernameBS passwordBS
      contentType = "application/x-www-form-urlencoded"
      content = "grant_type=client_credentials"
      payload = WTypes.Raw contentType $ HTTP.RequestBodyBS content
  response <- postWith options' fullUrl payload
  if response ^. responseStatus . statusCode == 200 then
    let body = response ^. responseBody
        accessToken = decode body
    in return accessToken
  else
    return Nothing

fetchAccessTokenWithExpiration :: EnvironmentUrl -> ClientID -> Secret ->
                                  IO (Maybe AccessTokenWithExpiration)
fetchAccessTokenWithExpiration environment username password= do
  currentTime <- getCurrentTime
  mayAccessToken <- fetchAccessToken environment username password
  let getExpire accToken = (accToken, safeExpirationTime currentTime accToken)
  return $ maybe Nothing (Just . getExpire) mayAccessToken

-- |Time at which the token should be considered expired. This is a few seconds
-- before the time that PayPal gives us. Parameters are the time at which the
-- access token was retrieved and the access token.
safeExpirationTime :: UTCTime -> AccessToken -> UTCTime
safeExpirationTime currentTime token =
  let safetyBuffer = 10 -- Seconds.
      seconds = aTokenExpires token - safetyBuffer
  in addUTCTime (fromIntegral seconds) currentTime
