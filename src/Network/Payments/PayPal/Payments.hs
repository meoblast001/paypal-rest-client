-- |
-- Module: Network.Payments.PayPal.Payments
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Network.Payments.PayPal.Payments
( URL
, Intent(..)
, CreditCardType(..)
, BillingAddress(..)
, CreditCard(..)
, FundingInstrument(..)
, ShippingAddressType(..)
, ShippingAddress(..)
, Payer(..)
, Details(..)
, Amount(..)
, Item(..)
, ItemList(..)
, Transaction(..)
, CreateRequest(..)
) where

import Data.CountryCodes

type URL = String

data Intent = SaleIntent | AuthoriseIntent | OrderIntent deriving (Show)

data CreditCardType = VisaCC | MasterCardCC | DiscoverCC | AMEXCC
  deriving (Show)

data BillingAddress = BillingAddress
  { billingAddressLine1 :: String
  , billingAddressLine2 :: Maybe String
  , billingAddressCity :: String
  , billingCountryCode :: CountryCode
  , billingPostalCode :: Maybe String
  , billingState :: Maybe String
  , billingPhone :: String
  } deriving (Show)

data CreditCard = CreditCard
  { creditCardNumber :: String
  , creditCardType :: CreditCardType
  , creditCardExpireMonth :: Int
  , creditCardExpireYear :: Int
  , creditCardCCV2 :: Maybe String
  , creditCardFirstName :: Maybe String
  , creditCardLastName :: Maybe String
  , creditCardBillingAddress :: Maybe BillingAddress
  } deriving (Show)

data FundingInstrument = FundingInstrument
  { fundInstCreditCard :: CreditCard
  } deriving (Show)

data ShippingAddressType =
  ShipAddrResidential | ShipAddrBusiness | ShipAddrMailbox deriving (Show)

data ShippingAddress = ShippingAddress
  { shipAddrRecipientName :: String
  , shipAddrType :: ShippingAddressType
  , shipAddrLine1 :: String
  , shipAddrLine2 :: Maybe String
  , shipAddrCity :: String
  , shipAddrCountryCode :: CountryCode
  , shipAddrPostalCode :: Maybe String
  , shipAddrState :: Maybe String
  , shipAddrPhone :: String
  } deriving (Show)

data Payer = Payer
  { payerFundingInstrument :: [FundingInstrument]
  , payerEmail :: String
  , payerSalutation :: String
  , payerFirstName :: String
  , payerMiddleName :: String
  , payerLastName :: String
  , payerSuffix :: String
  , payerPhone :: String
  , payerCountryCode :: String
  , payerShippingAddress :: ShippingAddress
  } deriving (Show)

data Details = Details
  { detailsShipping :: String
  , detailsSubtotal :: String
  , detailsTax :: String
  } deriving (Show)

data Amount = Amount
  { currency :: String
  , amountTotal :: String
  , amountDetails :: Details
  } deriving (Show)

data Item = Item
  { itemQuantity :: Integer
  , itemName :: String
  , itemPrice :: String
  , itemCurrency :: String
  , itemSku :: String
  , itemDescription :: String
  } deriving (Show)

data ItemList = ItemList
  { itemListItems :: [Item]
  , itemListShippingAddress :: Maybe ShippingAddress
  } deriving (Show)

data Transaction = Transaction
  { transactAmount :: Amount
  , transactDescription :: String
  , transactItemList :: ItemList
  } deriving (Show)

data CreateRequest = CreateRequest
  { createReqIntent :: Intent
  , createReqPayer :: Payer
  , createReqTransactions :: [Transaction]
  } deriving (Show)
