-- |
-- Module: Network.Payments.PayPal
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal
( HttpMethod(..)
, PayPalOperations(..)
, execPayPal
) where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Network.Payments.PayPal.Auth
import Network.Payments.PayPal.Environment
import Network.Wreq

-- |HTTP method (GET/POST).
data HttpMethod = HttpGet | HttpPost deriving (Show)

-- |A monad composing multiple PayPal operations which are to be performed.
-- The result can be executed using the execPayPal function.
data PayPalOperations :: * -> * where
  PPOPure :: a -> PayPalOperations a
  PPOBind :: PayPalOperations a -> (a -> PayPalOperations b) ->
             PayPalOperations b
  PayPalOperation :: FromJSON a =>
                     { ppoMethod :: HttpMethod
                     , ppoUrl :: String
                     , ppoOptions :: Options
                     , ppoPayload :: Payload
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

data PayPalError = NoAccessToken | ResponseParseError ErrorMessage JSONText
  deriving (Show)

-- |Authenticate with PayPal and then interact with the service.
execPayPal :: FromJSON a => EnvironmentUrl -> ClientID -> Secret ->
              PayPalOperations a -> IO (Either PayPalError a)
execPayPal envUrl username password operations = do
  mayAccessToken <- fetchAccessToken envUrl username password
  case mayAccessToken of
    Just accessToken -> execOpers envUrl accessToken operations
    Nothing -> return $ Left NoAccessToken

execOpers :: EnvironmentUrl -> AccessToken -> PayPalOperations a ->
             IO (Either PayPalError a)
execOpers _ _ (PPOPure a) = return $ Right a
execOpers envUrl' accessToken (PPOBind m f) = do
  treeLeftResult <- execOpers envUrl' accessToken m
  either (return . Left) (\res -> execOpers envUrl' accessToken $ f res)
         treeLeftResult
execOpers (EnvironmentUrl baseUrl) accessToken
          (PayPalOperation method url preOptions payload) = do
  let accToken = aToken accessToken
      opts = preOptions &
             header "Authorization" .~ [BS8.pack ("Bearer " ++ accToken)]
  response <- case method of
    HttpGet -> getWith opts (baseUrl ++ url)
    HttpPost -> postWith opts (baseUrl ++ url) payload
  let responseText = response ^. responseBody
  case eitherDecode responseText of
    Left errMsg -> return $ Left $ ResponseParseError errMsg responseText
    Right result -> return $ Right result
