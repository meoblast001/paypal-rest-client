-- |
-- Module: Network.Payments.PayPal.Types.Currency
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Payments.PayPal.Types.Currency
( Currency(..)
, MonetaryAmount(..)
) where

import Control.Monad
import Data.Decimal
import qualified Data.Text as T
import Data.Aeson
import Safe

-- |Currencies supported by PayPal.
data Currency =
  AUD | BRL | CAD | CZK | DKK | EUR | HKD | HUF | ILS | JPY | MYR | MXN | TWD |
  NZD | NOK | PHP | PLN | GBP | RUB | SGD | SEK | CHF | THB | TRY | USD
  deriving (Eq, Read, Show)

instance ToJSON Currency where
  toJSON currency = String $ T.pack $ show currency

instance FromJSON Currency where
  parseJSON (String txt) = maybe mzero return $ readMay $ T.unpack txt
  parseJSON _ = mzero

-- |Type based on Decimal which is used for holding monetary amounts and can be
-- encoded to and decoded from JSON. Encodes to a string, decodes from string or
-- number.
newtype MonetaryAmount = MonetaryAmount Decimal
  deriving (Eq, Num, Ord, Read, Real, Show)

instance ToJSON MonetaryAmount where
  toJSON (MonetaryAmount value) = String $ T.pack $ show $ roundTo 2 value

instance FromJSON MonetaryAmount where
  parseJSON (String value) =
    return $ MonetaryAmount $ roundTo 2 $ read $ T.unpack value
  parseJSON (Number value) = return $ MonetaryAmount $ realFracToDecimal 2 value
  parseJSON _ = mzero
