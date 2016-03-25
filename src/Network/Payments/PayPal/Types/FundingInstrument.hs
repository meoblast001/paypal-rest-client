-- |
-- Module: Network.Payments.PayPal.Types.FundingInstrument
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Types.FundingInstrument
( CreditCardType(..)
, CreditCard(..)
, FundingInstrument(..)
) where

import Control.Monad
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import Network.Payments.PayPal.Types.Address

-- |Type of credit card being used.
data CreditCardType = VisaCC | MasterCardCC | DiscoverCC | AMEXCC
  deriving (Show)

instance ToJSON CreditCardType where
  toJSON VisaCC = "visa"
  toJSON MasterCardCC = "mastercard"
  toJSON DiscoverCC = "discover"
  toJSON AMEXCC = "amex"

instance FromJSON CreditCardType where
  parseJSON (String text) =
    -- Lower case is documented but sometimes PayPal likes to return upper
    -- case...
    case T.toLower text of
      "visa" -> return VisaCC
      "mastercard" -> return MasterCardCC
      "discover" -> return DiscoverCC
      "amex" -> return AMEXCC
      _ -> mzero
  parseJSON _ = mzero

-- |Information about a credit card.
data CreditCard = CreditCard
  { creditCardNumber :: String
  , creditCardType :: CreditCardType
  , creditCardExpireMonth :: Int
  , creditCardExpireYear :: Int
  , creditCardCVV2 :: Maybe String
  , creditCardFirstName :: Maybe String
  , creditCardLastName :: Maybe String
  , creditCardBillingAddress :: Maybe Address
  } deriving (Show)

instance ToJSON CreditCard where
  toJSON cc =
    object (["number" .= creditCardNumber cc,
             "type" .= creditCardType cc,
             "expire_month" .= show (creditCardExpireMonth cc),
             "expire_year" .= show (creditCardExpireYear cc)] ++
            maybeToList (("cvv2" .=) <$> creditCardCVV2 cc) ++
            maybeToList (("first_name" .=) <$> creditCardFirstName cc) ++
            maybeToList (("last_name" .=) <$> creditCardLastName cc) ++
            maybeToList (("billing_address" .=) <$>
                         creditCardBillingAddress cc))

instance FromJSON CreditCard where
  parseJSON (Object obj) =
    CreditCard <$>
    obj .: "number" <*>
    obj .: "type" <*>
    fmap read (obj .: "expire_month") <*>
    fmap read (obj .: "expire_year") <*>
    obj .:? "ccv2" <*>
    obj .:? "first_name" <*>
    obj .:? "last_name" <*>
    obj .:? "billing_address"
  parseJSON _ = mzero

-- |Representation of either a new credit card or existing credit card data.
data FundingInstrument = FundingInstrument
  { fundInstCreditCard :: Maybe CreditCard
  } deriving (Show)

instance ToJSON FundingInstrument where
  toJSON fundInstr =
    object (maybeToList (("credit_card" .=) <$> fundInstCreditCard fundInstr))

instance FromJSON FundingInstrument where
  parseJSON (Object obj) = FundingInstrument <$> obj .:? "credit_card"
  parseJSON _ = mzero
