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
, PayerInfo(..)
, Payer(..)
, Details(..)
, Amount(..)
, Item(..)
, ItemList(..)
, Transaction(..)
, CreateRequest(..)
, HateoasLink(..)
, CreateResponse(..)
, createPayment
) where

import Control.Monad
import Data.Aeson
import Data.CountryCodes
import Data.Maybe
import qualified Network.HTTP.Client as HTTP
import Network.Payments.PayPal
import Network.Wreq
import qualified Network.Wreq.Types as WTypes

type URL = String

data Intent = SaleIntent | AuthoriseIntent | OrderIntent deriving (Show)

instance ToJSON Intent where
  toJSON SaleIntent = "sale"
  toJSON AuthoriseIntent = "authorize"
  toJSON OrderIntent = "order"

instance FromJSON Intent where
  parseJSON (String "sale") = return SaleIntent
  parseJSON (String "authorize") = return AuthoriseIntent
  parseJSON (String "order") = return OrderIntent
  parseJSON _ = mzero

data CreditCardType = VisaCC | MasterCardCC | DiscoverCC | AMEXCC
  deriving (Show)

instance ToJSON CreditCardType where
  toJSON VisaCC = "visa"
  toJSON MasterCardCC = "mastercard"
  toJSON DiscoverCC = "discover"
  toJSON AMEXCC = "amex"

instance FromJSON CreditCardType where
  parseJSON (String "visa") = return VisaCC
  parseJSON (String "mastercard") = return MasterCardCC
  parseJSON (String "discover") = return DiscoverCC
  parseJSON (String "amex") = return AMEXCC
  parseJSON _ = mzero

data PaymentState = PayStateCreated | PayStateApproved | PayStateFailed |
                    PayStateCancelled | PayStateExpired | PayStatePending
                    deriving (Show)

instance FromJSON PaymentState where
  parseJSON (String "created") = return PayStateCreated
  parseJSON (String "approved") = return PayStateApproved
  parseJSON (String "failed") = return PayStateFailed
  parseJSON (String "canceled") = return PayStateCancelled
  parseJSON (String "expired") = return PayStateExpired
  parseJSON (String "pending") = return PayStatePending

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

data FundingInstrument = FundingInstrument
  { fundInstCreditCard :: CreditCard
  } deriving (Show)

instance ToJSON FundingInstrument where
  toJSON fundInstr = object ["credit_card" .= fundInstCreditCard fundInstr]

instance FromJSON FundingInstrument where
  parseJSON (Object obj) = FundingInstrument <$> obj .: "credit_card"
  parseJSON _ = mzero

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
             "country_code" .= toText (shipAddrCountryCode addr),
             "phone" .= shipAddrPhone addr] ++
            maybeToList (("line2" .=) <$> shipAddrLine2 addr) ++
            maybeToList (("postal_code" .=) <$> shipAddrPostalCode addr) ++
            maybeToList (("state" .=) <$> shipAddrState addr))

data PayerInfo = PayerInfo
  { payerInfoEmail :: String
  } deriving (Show)

instance ToJSON PayerInfo where
  toJSON info = object ["email" .= payerInfoEmail info]

instance FromJSON PayerInfo where
  parseJSON (Object obj) = PayerInfo <$> obj .: "email"
  parseJSON _ = mzero

data Payer = Payer
  { payerFundingInstruments :: [FundingInstrument]
  , payerInfo :: Maybe PayerInfo
  } deriving (Show)

instance ToJSON Payer where
  toJSON payer =
    -- TODO: Support something other than credit card.
    object (["payment_method" .= ("credit_card" :: String),
             "funding_instruments" .= payerFundingInstruments payer] ++
            maybeToList (("payer_info" .=) <$> payerInfo payer))

instance FromJSON Payer where
  parseJSON (Object obj) =
    Payer <$>
    obj .: "funding_instruments" <*>
    obj .:? "payer_info"
  parseJSON _ = mzero

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
            maybeToList (("shipping_address" .=) <$>
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

data HateoasLink = HateoasLink
  { hateoasHref :: URL
  , hateoasRel :: String
  , hateoasMethod :: HttpMethod
  } deriving (Show)

instance FromJSON HateoasLink where
  parseJSON (Object obj) =
    HateoasLink <$>
    obj .: "href" <*>
    obj .: "rel" <*>
    ((obj .: "method") >>= parseHttpMethod)
    where
      parseHttpMethod (String "GET") = return HttpGet
      parseHttpMethod (String "POST") = return HttpPost
      parseHttpMethod _ = mzero
  parseJSON _ = mzero

data CreateResponse = CreateResponse
  { createResIntent :: Intent
  , createResPayer :: Payer
  , createResPayState :: PaymentState
  , createResHateoasLinks :: [HateoasLink]
  } deriving (Show)

instance FromJSON CreateResponse where
  parseJSON (Object obj) =
    CreateResponse <$>
    obj .: "intent" <*>
    obj .: "payer" <*>
    obj .: "state" <*>
    obj .: "links"
  parseJSON _ = mzero

createPayment :: CreateRequest -> PayPalOperations CreateResponse
createPayment request =
  let url = "/v1/payments/payment"
      contentType = "application/json"
      content = encode request
      payload = WTypes.Raw contentType $ HTTP.RequestBodyLBS content
  in PayPalOperation HttpPost url defaults payload
