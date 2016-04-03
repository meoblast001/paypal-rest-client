-- |
-- Module: Network.Payments.PayPal.Types.Currency
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Types.Currency (Currency(..)) where

import Control.Monad
import qualified Data.Text as T
import Data.Aeson
import Safe

-- |Currencies supported by PayPal.
data Currency =
  AUD | BRL | CAD | CZK | DKK | EUR | HKD | HUF | ILS | JPY | MYR | MXN | TWD |
  NZD | NOK | PHP | PLN | GBP | RUB | SGD | SEK | CHF | THB | TRY | USD
  deriving (Read, Show)

instance ToJSON Currency where
  toJSON currency = String $ T.pack $ show currency

instance FromJSON Currency where
  parseJSON (String txt) = maybe mzero return $ readMay $ T.unpack txt
  parseJSON _ = mzero
