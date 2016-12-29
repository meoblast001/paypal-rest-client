-- |
-- Module: Network.Payments.PayPal
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal
( UseHttpMethod(..)
, PayPalOperations(..)
, JSONText
, ErrorMessage
, PayPalError(..)
, execPayPal
, execPayPalOpers
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Exception
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock
import qualified Network.HTTP.Client as HTTPClient
import Network.Payments.PayPal.Auth
import Network.Payments.PayPal.Environment
import Network.Wreq

-- |HTTP method to use in the request (GET/POST).
data UseHttpMethod = UseHttpGet | UseHttpPost Payload | UseHttpPatch Payload

-- Instance of show that ignores the payload.
instance Show UseHttpMethod where
  show UseHttpGet = "HttpGet"
  show (UseHttpPost _) = "HttpPost"
  show (UseHttpPatch _) = "HttpPatch"

-- |A monad composing multiple PayPal operations which are to be performed.
-- The result can be executed using the execPayPal function.
data PayPalOperations :: * -> * where
  PPOPure :: a -> PayPalOperations a
  PPOBind :: PayPalOperations a -> (a -> PayPalOperations b) ->
             PayPalOperations b
  PayPalOperation :: FromJSON a =>
                     { ppoMethod :: UseHttpMethod
                     , ppoUrl :: String
                     , ppoOptions :: Options
                     } -> PayPalOperations a

instance Functor PayPalOperations where
  fmap f m = PPOBind m (PPOPure . f)

instance Applicative PayPalOperations where
  pure x = PPOPure x
  mf <*> mx = PPOBind mf (\f -> PPOBind mx (\x -> PPOPure (f x)))

instance Monad PayPalOperations where
  m >>= f = PPOBind m f

type JSONText = LBS.ByteString
type ErrorMessage = String

data PayPalError = HttpStatusError Int |
                   ResponseParseError ErrorMessage JSONText |
                   HttpError HTTPClient.HttpException | OtherError String
                   deriving (Show)

-- |Authenticate with PayPal and then interact with the service.
execPayPal :: FromJSON a => EnvironmentUrl -> ClientID -> Secret ->
              PayPalOperations a -> IO (Either PayPalError a)
execPayPal envUrl username password operations = do
  accessTokenOrErr <- fetchAccessTokenWithExpiration envUrl username password
  case accessTokenOrErr of
    -- Failure to fetch access token.
    Left (AccessTokenHttpError httpErr) -> return $ Left $ HttpError httpErr
    Left (AccessTokenStatusError statusCode') ->
      return $ Left $ HttpStatusError statusCode'
    Left (AccessTokenParseError errMsg text) ->
      return $ Left $ ResponseParseError errMsg text
    -- Successfully acquired, execute operations.
    Right accTokenWithEx -> do
      result <- execPayPalOpers envUrl username password accTokenWithEx
                                operations
      case result of
        Left err -> return $ Left err
        Right (result', _) -> return $ Right result'

-- |Executes a PayPalOperations monad as IO. Because the access token can
-- expire and needs to be renewed, this function returns the desired value and
-- the most current access token when successful.
execPayPalOpers :: EnvironmentUrl -> ClientID -> Secret ->
                   AccessTokenWithExpiration -> PayPalOperations a ->
                   IO (Either PayPalError (a, AccessTokenWithExpiration))
execPayPalOpers _ _ _ accTokenWithEx (PPOPure a) =
  return $ Right (a, accTokenWithEx)
execPayPalOpers envUrl' username password accTokenWithEx (PPOBind m f) = do
  treeLeftResult <- execPayPalOpers envUrl' username password accTokenWithEx m
  either (return . Left)
         (\(res, newAccTk) -> execPayPalOpers envUrl' username password
                                              newAccTk $ f res)
         treeLeftResult
execPayPalOpers env@(EnvironmentUrl baseUrl) username password
          accTokenWithEx@(accessToken, expiration)
          (PayPalOperation method url preOptions) = do
  -- Check the validity of the access token and renew it if it expired.
  curTime <- getCurrentTime
  latestAccTkOrErr <- if diffUTCTime expiration curTime <= 0
                      then fetchAccessTokenWithExpiration env username password
                      else return $ Right accTokenWithEx
  case latestAccTkOrErr of
    -- Failure to refresh access token.
    Left (AccessTokenHttpError httpErr) -> return $ Left $ HttpError httpErr
    Left (AccessTokenStatusError statusCode') ->
      return $ Left $ HttpStatusError statusCode'
    Left (AccessTokenParseError errMsg text) ->
      return $ Left $ ResponseParseError errMsg text
    -- Either existing access token is still valid or new access token was
    -- retrieved.
    Right latestAccTk -> do
      -- Perform the request.
      let accToken = aToken accessToken
          opts = preOptions &
                 header "Authorization" .~ [BS8.pack ("Bearer " ++ accToken)]
      responseOrErr <- try $ case method of
        UseHttpGet -> getWith opts (baseUrl ++ url)
        UseHttpPost payload -> postWith opts (baseUrl ++ url) payload
        UseHttpPatch payload ->
          customPayloadMethodWith "PATCH" opts (baseUrl ++ url) payload
      case responseOrErr of
        -- HTTP request failed.
        Left err -> return $ Left $ HttpError err
        -- HTTP request successful.
        Right response -> do
          let statusCode' = response ^. responseStatus . statusCode
          if statusCode' == 200 then
            let responseText = response ^. responseBody
            in case eitherDecode responseText of
              Left errMsg ->
                return $ Left $ ResponseParseError errMsg responseText
              Right result -> return $ Right (result, latestAccTk)
          else
            return $ Left $ HttpStatusError statusCode'
