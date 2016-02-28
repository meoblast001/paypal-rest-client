-- |
-- Module: Network.Payments.PayPal.Payments
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Payments
( URL
, Intent(..)
, CreditCardType(..)
, Address(..)
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
, createPayment
) where

import Control.Lens hiding ((.=))
import Data.Aeson.Encode
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy
import Data.CountryCodes
import Data.Maybe
import qualified Network.HTTP.Client as HTTP
import Network.Payments.PayPal
import Network.Payments.PayPal.Auth
import Network.Payments.PayPal.Environment
import Network.Wreq
import qualified Network.Wreq.Types as WTypes

type URL = String

data Intent = SaleIntent | AuthoriseIntent | OrderIntent deriving (Show)

instance ToJSON Intent where
  toJSON SaleIntent = "sale"
  toJSON AuthoriseIntent = "authorize"
  toJSON OrderIntent = "order"

data CreditCardType = VisaCC | MasterCardCC | DiscoverCC | AMEXCC
  deriving (Show)

instance ToJSON CreditCardType where
  toJSON VisaCC = "visa"
  toJSON MasterCardCC = "mastercard"
  toJSON DiscoverCC = "discover"
  toJSON AMEXCC = "amex"

data Address = Address
  { addressLine1 :: String
  , addressLine2 :: Maybe String
  , addressCity :: String
  , addressCountryCode :: CountryCode
  , addressPostalCode :: Maybe String
  , addressState :: Maybe String
  , addressPhone :: String
  } deriving (Show)

instance ToJSON Address where
  toJSON addr =
    object (["line1" .= addressLine1 addr,
             "city" .= addressCity addr,
             "country_code" .= (toText $ addressCountryCode addr),
             "phone" .= addressPhone addr] ++
            maybeToList (fmap ("line2" .=) $ addressLine2 addr) ++
            maybeToList (fmap ("postal_code" .=) $ addressPostalCode addr) ++
            maybeToList (fmap ("state" .=) $ addressState addr))

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
             "expire_month" .= (show $ creditCardExpireMonth cc),
             "expire_year" .= (show $ creditCardExpireYear cc)] ++
            maybeToList (fmap ("cvv2" .=) $ creditCardCVV2 cc) ++
            maybeToList (fmap ("first_name" .=) $ creditCardFirstName cc) ++
            maybeToList (fmap ("last_name" .=) $ creditCardLastName cc) ++
            maybeToList (fmap ("billing_address" .=) $
                              creditCardBillingAddress cc))

data FundingInstrument = FundingInstrument
  { fundInstCreditCard :: CreditCard
  } deriving (Show)

instance ToJSON FundingInstrument where
  toJSON fundInstr = object ["credit_card" .= fundInstCreditCard fundInstr]

data ShippingAddressType =
  ShipAddrResidential | ShipAddrBusiness | ShipAddrMailbox deriving (Show)

instance ToJSON ShippingAddressType where
  toJSON ShipAddrResidential = "residential"
  toJSON ShipAddrBusiness = "business"
  toJSON ShipAddrMailbox = "mailbox"

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

instance ToJSON ShippingAddress where
  toJSON addr =
    object (["recipient_name" .= shipAddrRecipientName addr,
             "type" .= shipAddrType addr,
             "line1" .= shipAddrLine1 addr,
             "city" .= shipAddrCity addr,
             "country_code" .= (toText $ shipAddrCountryCode addr),
             "phone" .= shipAddrPhone addr] ++
            maybeToList (fmap ("line2" .=) $ shipAddrLine2 addr) ++
            maybeToList (fmap ("postal_code" .=) $ shipAddrPostalCode addr) ++
            maybeToList (fmap ("state" .=) $ shipAddrState addr))

data Payer = Payer
  { payerFundingInstruments :: [FundingInstrument]
  , payerEmail :: String
  , payerSalutation :: String
  , payerFirstName :: String
  , payerMiddleName :: String
  , payerLastName :: String
  , payerSuffix :: String
  , payerPhone :: String
  , payerCountryCode :: CountryCode
  , payerShippingAddress :: ShippingAddress
  } deriving (Show)

instance ToJSON Payer where
  toJSON payer =
    let payerInfo = object ["email" .= payerEmail payer,
                            "salutation" .= payerSalutation payer,
                            "first_name" .= payerSalutation payer,
                            "middle_name" .= payerMiddleName payer,
                            "last_name" .= payerLastName payer,
                            "suffix" .= payerSuffix payer,
                            "phone" .= payerPhone payer,
                            "country_code" .= (toText $ payerCountryCode payer),
                            "shipping_address" .= payerShippingAddress payer]
    -- TODO: Support something other than credit card.
    in object ["payment_method" .= ("credit_card" :: String),
               "funding_instruments" .= payerFundingInstruments payer,
               "payer_info" .= payerInfo]

data Details = Details
  { detailsShipping :: String
  , detailsSubtotal :: String
  , detailsTax :: String
  } deriving (Show)

instance ToJSON Details where
  toJSON details =
    object ["shipping" .= detailsShipping details,
            "subtotal" .= detailsSubtotal details,
            "tax" .= detailsTax details]

data Amount = Amount
  { amountCurrency :: String
  , amountTotal :: String
  , amountDetails :: Details
  } deriving (Show)

instance ToJSON Amount where
  toJSON amt =
    object ["currency" .= amountCurrency amt,
            "total" .= amountTotal amt,
            "details" .= amountDetails amt]

data Item = Item
  { itemQuantity :: Integer
  , itemName :: String
  , itemPrice :: String
  , itemCurrency :: String
  , itemSku :: String
  , itemDescription :: String
  } deriving (Show)

instance ToJSON Item where
  toJSON item =
    object ["quantity" .= itemQuantity item,
            "name" .= itemName item,
            "price" .= itemPrice item,
            "currency" .= itemCurrency item,
            "sku" .= itemSku item,
            "description" .= itemDescription item]

data ItemList = ItemList
  { itemListItems :: [Item]
  , itemListShippingAddress :: Maybe ShippingAddress
  } deriving (Show)

instance ToJSON ItemList where
  toJSON list =
    object (["items" .= itemListItems list] ++
            maybeToList (fmap ("shipping_address" .=) $
                              itemListShippingAddress list))

data Transaction = Transaction
  { transactAmount :: Amount
  , transactDescription :: String
  , transactItemList :: ItemList
  } deriving (Show)

instance ToJSON Transaction where
  toJSON trans =
    object ["amount" .= transactAmount trans,
            "description" .= transactDescription trans,
            "item_list" .= transactItemList trans]

data CreateRequest = CreateRequest
  { createReqIntent :: Intent
  , createReqPayer :: Payer
  , createReqTransactions :: [Transaction]
  } deriving (Show)

instance ToJSON CreateRequest where
  toJSON req =
    object ["intent" .= createReqIntent req,
            "payer" .= createReqPayer req,
            "transactions" .= createReqTransactions req]

-- TODO: Define the actual response as a parsed type.
type CreateResponse = ByteString

createPayment :: PayPalSession -> CreateRequest -> IO (Maybe CreateResponse)
createPayment session request = do
  let (EnvironmentUrl baseUrl) = ppSessionEnvironment session
      fullUrl = baseUrl ++ "/v1/payments/payment"
      accToken = aToken $ ppAccessToken session
      options = defaults &
                header "Authorization" .~ [BS8.pack ("Bearer " ++ accToken)]
      contentType = "application/json"
      content = encode request
      payload = WTypes.Raw contentType $ HTTP.RequestBodyLBS content
  response <- postWith options fullUrl payload
  if response ^. responseStatus . statusCode == 200 then
    return $ Just (response ^. responseBody)
  else
    return Nothing
