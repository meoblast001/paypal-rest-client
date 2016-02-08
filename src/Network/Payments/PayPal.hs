-- |
-- Module: Network.Payments.PayPal
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Network.Payments.PayPal
( PayPalSession(..)
, withPayPalSession
) where

import Data.Functor
import Network.Payments.PayPal.Auth
import Network.Payments.PayPal.Environment

-- |Data related to a session with PayPal.
data PayPalSession = PayPalSession
  { ppSessionEnvironment :: EnvironmentUrl
  , ppAccessToken :: AccessToken
  } deriving (Show)

-- |Authenticate with PayPal and then interact with the service.
withPayPalSession :: EnvironmentUrl -> ClientID -> Secret ->
                     (PayPalSession -> IO a) -> IO (Maybe a)
withPayPalSession envUrl username password callback = do
  mayAccessToken <- fetchAccessToken envUrl username password
  case mayAccessToken of
    Just accessToken ->
      let session = PayPalSession envUrl accessToken
      in Just <$> callback session
    Nothing -> return Nothing
