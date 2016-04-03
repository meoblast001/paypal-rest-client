-- |
-- Module: Network.Payments.PayPal.Types.Payer
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Types.Payer
( PayerInfo(..)
, PaymentMethod(..)
, PayerStatus(..)
, Payer(..)
) where

import Control.Monad
import Data.Aeson
import qualified Data.Foldable as F
import Data.Maybe
import Network.Payments.PayPal.Types.FundingInstrument

-- |Optional additional information about the payer.
data PayerInfo = PayerInfo
  { payerInfoEmail :: String
  } deriving (Show)

instance ToJSON PayerInfo where
  toJSON info = object ["email" .= payerInfoEmail info]

instance FromJSON PayerInfo where
  parseJSON (Object obj) = PayerInfo <$> obj .: "email"
  parseJSON _ = mzero

-- |Method of payment.
data PaymentMethod = PayMethodPayPal | PayMethodCreditCard deriving (Show)

instance ToJSON PaymentMethod where
  toJSON PayMethodPayPal = "paypal"
  toJSON PayMethodCreditCard = "credit_card"

instance FromJSON PaymentMethod where
  parseJSON (String "paypal") = return PayMethodPayPal
  parseJSON (String "credit_card") = return PayMethodCreditCard
  parseJSON _ = mzero

-- |Account verification status of the payer.
data PayerStatus = PayerStatusVerified | PayerStatusUnverified deriving (Show)

instance FromJSON PayerStatus where
  parseJSON (String "VERIFIED") = return PayerStatusVerified
  parseJSON (String "UNVERIFIED") = return PayerStatusUnverified
  parseJSON _ = mzero

instance ToJSON PayerStatus where
  toJSON PayerStatusVerified = "VERIFIED"
  toJSON PayerStatusUnverified = "UNVERIFIED"

-- |Information about the payer in a transaction.
data Payer = Payer
  { payerPaymentMethod :: PaymentMethod
  , payerFundingInstruments :: [FundingInstrument]
  , payerInfo :: Maybe PayerInfo
  , payerStatus :: Maybe PayerStatus
  } deriving (Show)

instance ToJSON Payer where
  toJSON payer =
    let fundingInstr = if null $ payerFundingInstruments payer then Nothing
                       else Just $ payerFundingInstruments payer
    in object (["payment_method" .= payerPaymentMethod payer] ++
               maybeToList (("funding_instruments" .=) <$> fundingInstr) ++
               maybeToList (("payer_info" .=) <$> payerInfo payer) ++
               maybeToList (("status" .=) <$> payerStatus payer))

instance FromJSON Payer where
  parseJSON (Object obj) =
    Payer <$>
    obj .: "payment_method" <*>
    (obj .:? "funding_instruments" >>= return . F.concat) <*>
    obj .:? "payer_info" <*>
    obj .:? "status"
  parseJSON _ = mzero
