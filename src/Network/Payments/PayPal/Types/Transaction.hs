-- |
-- Module: Network.Payments.PayPal.Types.Transaction
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Types.Transaction
( Details(..)
, Amount(..)
, Item(..)
, ItemList(..)
, Transaction(..)
) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Text as T
import Network.Payments.PayPal.Types.Address
import Network.Payments.PayPal.Types.Currency

-- |Details about the amount of a transaction.
data Details = Details
  { detailsShipping :: MonetaryAmount
  , detailsSubtotal :: MonetaryAmount
  , detailsTax :: MonetaryAmount
  } deriving (Eq, Show)

instance ToJSON Details where
  toJSON details =
    object ["shipping" .= detailsShipping details,
            "subtotal" .= detailsSubtotal details,
            "tax" .= detailsTax details]

instance FromJSON Details where
  parseJSON (Object obj) =
    Details <$>
    obj .: "shipping" <*>
    obj .: "subtotal" <*>
    obj .: "tax"
  parseJSON _ = mzero

-- |Amount of a transaction and its currency. The details must sum up to the
-- total or the request is rejected.
data Amount = Amount
  { amountCurrency :: Currency
  , amountTotal :: MonetaryAmount
  , amountDetails :: Details
  } deriving (Eq, Show)

instance ToJSON Amount where
  toJSON amt =
    object ["currency" .= amountCurrency amt,
            "total" .= amountTotal amt,
            "details" .= amountDetails amt]

instance FromJSON Amount where
  parseJSON (Object obj) =
    Amount <$>
    obj .: "currency" <*>
    obj .: "total" <*>
    obj .: "details"
  parseJSON _ = mzero

-- |An individual item being purchased.
data Item = Item
  { itemQuantity :: Integer
  , itemName :: String
  , itemPrice :: MonetaryAmount
  , itemCurrency :: Currency
  , itemSku :: String
  , itemDescription :: Maybe String
  } deriving (Eq, Show)

instance ToJSON Item where
  toJSON item =
    object (["quantity" .= itemQuantity item,
             "name" .= itemName item,
             "price" .= itemPrice item,
             "currency" .= itemCurrency item,
             "sku" .= itemSku item] ++
            maybeToList (("description" .=) <$> itemDescription item))

instance FromJSON Item where
  parseJSON (Object obj) =
    Item <$>
    (obj .: "quantity" >>= parseFuzzyJSONInt) <*>
    obj .: "name" <*>
    obj .: "price" <*>
    obj .: "currency" <*>
    obj .: "sku" <*>
    obj .:? "description"
  parseJSON _ = mzero

-- |A list of items being purchased and the shipping address if one exists.
data ItemList = ItemList
  { itemListItems :: [Item]
  , itemListShippingAddress :: Maybe ShippingAddress
  } deriving (Eq, Show)

instance ToJSON ItemList where
  toJSON list =
    object (["items" .= itemListItems list] ++
            maybeToList (("shipping_address" .=) <$>
                         itemListShippingAddress list))

instance FromJSON ItemList where
  parseJSON (Object obj) =
    ItemList <$>
    obj .: "items" <*>
    obj .:? "shipping_address"
  parseJSON _ = mzero

-- |Details about a financial transaction over PayPal.
data Transaction = Transaction
  { transactAmount :: Amount
  , transactDescription :: Maybe String
  , transactItemList :: ItemList
  } deriving (Eq, Show)

instance ToJSON Transaction where
  toJSON trans =
    object (["amount" .= transactAmount trans,
             "item_list" .= transactItemList trans] ++
            maybeToList (("description" .=) <$> transactDescription trans))

instance FromJSON Transaction where
  parseJSON (Object obj) =
    Transaction <$>
    obj .: "amount" <*>
    obj .:? "description" <*>
    obj .: "item_list"
  parseJSON _ = mzero

-- |This takes either a string or a number and tries to produce an integer out
-- of it. This is necessary because PayPal apparently returns different types
-- for the same data depending on the usage...
parseFuzzyJSONInt :: (Integral a, Read a) => Value -> Parser a
parseFuzzyJSONInt (String txt) = return $ read $ T.unpack txt
parseFuzzyJSONInt (Number num) = return $ round num
parseFuzzyJSONInt _ = mzero
