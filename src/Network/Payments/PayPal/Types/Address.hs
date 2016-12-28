-- |
-- Module: Network.Payments.PayPal.Types.Address
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Types.Address
( Address(..)
, ShippingAddressType(..)
, ShippingAddress(..)
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Data.Aeson
import Data.CountryCodes
import Data.Maybe

-- Billing address of the payer.
data Address = Address
  { addressLine1 :: String
  , addressLine2 :: Maybe String
  , addressCity :: String
  , addressCountryCode :: CountryCode
  , addressPostalCode :: Maybe String
  , addressState :: Maybe String
  , addressPhone :: String
  } deriving (Eq, Show)

instance ToJSON Address where
  toJSON addr =
    object (["line1" .= addressLine1 addr,
             "city" .= addressCity addr,
             "country_code" .= toText (addressCountryCode addr),
             "phone" .= addressPhone addr] ++
            maybeToList (("line2" .=) <$> addressLine2 addr) ++
            maybeToList (("postal_code" .=) <$> addressPostalCode addr) ++
            maybeToList (("state" .=) <$> addressState addr))

instance FromJSON Address where
  parseJSON (Object obj) =
    Address <$>
    obj .: "line1" <*>
    obj .:? "line2" <*>
    obj .: "city" <*>
    (fmap fromMText (obj .: "country_code") >>= maybe mzero return) <*>
    obj .:? "postal_code" <*>
    obj .:? "state" <*>
    obj .: "phone"
  parseJSON _ = mzero

-- |The type of the address.
data ShippingAddressType =
  ShipAddrResidential | ShipAddrBusiness | ShipAddrMailbox
  deriving (Eq, Read, Show)

instance ToJSON ShippingAddressType where
  toJSON ShipAddrResidential = "residential"
  toJSON ShipAddrBusiness = "business"
  toJSON ShipAddrMailbox = "mailbox"

instance FromJSON ShippingAddressType where
  parseJSON (String "residential") = return ShipAddrResidential
  parseJSON (String "business") = return ShipAddrBusiness
  parseJSON (String "mailbox") = return ShipAddrMailbox
  parseJSON _ = mzero

-- |The payer's shipping address.
data ShippingAddress = ShippingAddress
  { shipAddrRecipientName :: Maybe String
  , shipAddrType :: Maybe ShippingAddressType
  , shipAddrLine1 :: String
  , shipAddrLine2 :: Maybe String
  , shipAddrCity :: String
  , shipAddrCountryCode :: CountryCode
  , shipAddrPostalCode :: Maybe String
  , shipAddrState :: Maybe String
  , shipAddrPhone :: Maybe String
  } deriving (Eq, Show)

instance ToJSON ShippingAddress where
  toJSON addr =
    object (maybeToList (("recipient_name" .=) <$>
                         shipAddrRecipientName addr) ++
            ["line1" .= shipAddrLine1 addr,
             "city" .= shipAddrCity addr,
             "country_code" .= toText (shipAddrCountryCode addr)] ++
            maybeToList (("type" .=) <$> shipAddrType addr) ++
            maybeToList (("line2" .=) <$> shipAddrLine2 addr) ++
            maybeToList (("postal_code" .=) <$> shipAddrPostalCode addr) ++
            maybeToList (("state" .=) <$> shipAddrState addr) ++
            maybeToList (("phone" .=) <$> shipAddrPhone addr))

instance FromJSON ShippingAddress where
  parseJSON (Object obj) =
    ShippingAddress <$>
    obj .:? "recipient_name" <*>
    obj .:? "type" <*>
    obj .: "line1" <*>
    obj .:? "line2" <*>
    obj .: "city" <*>
    obj .: "country_code" <*>
    obj .:? "postal_code" <*>
    obj .:? "state" <*>
    obj .:? "phone"
  parseJSON _ = mzero
