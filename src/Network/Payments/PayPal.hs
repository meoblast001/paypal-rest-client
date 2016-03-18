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

-- |Authenticate with PayPal and then interact with the service.
execPayPal :: FromJSON a => EnvironmentUrl -> ClientID -> Secret ->
              PayPalOperations a -> IO (Maybe a)
execPayPal envUrl username password operations = do
  mayAccessToken <- fetchAccessToken envUrl username password
  case mayAccessToken of
    Just accessToken -> execOpers envUrl accessToken operations
    Nothing -> return Nothing
  where
    execOpers :: EnvironmentUrl -> AccessToken -> PayPalOperations a ->
                 IO (Maybe a)
    execOpers _ _ (PPOPure a) = return $ Just a
    execOpers envUrl' accessToken (PPOBind m f) = do
      leftResult <- execOpers envUrl' accessToken m
      maybe (return Nothing) (\res -> execOpers envUrl' accessToken $ f res)
            leftResult
    execOpers (EnvironmentUrl baseUrl) accessToken
              (PayPalOperation method url preOptions payload) = do
      let accToken = aToken accessToken
          opts = preOptions &
                  header "Authorization" .~ [BS8.pack ("Bearer " ++ accToken)]
      response <- case method of
        HttpGet -> getWith opts (baseUrl ++ url)
        HttpPost -> postWith opts (baseUrl ++ url) payload
      return $ decode (response ^. responseBody)
