-- |
-- Module: Network.Payments.PayPal.Auth
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Network.Payments.PayPal.Environment
( EnvironmentUrl(..)
, sandboxUrl
, liveUrl
) where

-- |URL of a PayPal environment.
newtype EnvironmentUrl = EnvironmentUrl String deriving (Eq, Show)

-- |URL to sandbox environment.
sandboxUrl :: EnvironmentUrl
sandboxUrl = EnvironmentUrl "https://api.sandbox.paypal.com"

-- |URL to live environment.
liveUrl :: EnvironmentUrl
liveUrl = EnvironmentUrl "https://api.paypal.com"
