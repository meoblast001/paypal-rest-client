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
, PaymentID
, Intent(..)
, CreateRequest(..)
, CreateResponse(..)
, FindResponse(..)
, createPayment
, findById
) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Format
import qualified Network.HTTP.Client as HTTP
import Network.Payments.PayPal
import Network.Payments.PayPal.Hateoas
import Network.Payments.PayPal.Types.Payer
import Network.Payments.PayPal.Types.Transaction
import Network.Wreq
import qualified Network.Wreq.Types as WTypes

-- A string representing a URL.
type URL = String

-- The ID of a payment provided by PayPal.
type PaymentID = String

-- |Payment intent.
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

data RedirectUrls = RedirectUrls
  { redirUrlReturn :: URL
  , redirUrlCancel :: URL
  } deriving (Show)

instance FromJSON RedirectUrls where
  parseJSON (Object obj) =
    RedirectUrls <$>
    obj .: "return_url" <*>
    obj .: "cancel_url"
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
  parseJSON _ = mzero

-- |Contains data sent to PayPal to create a payment.
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

-- |Contains a parsed response from PayPal after making a create payment
-- request.
data CreateResponse = CreateResponse
  { createResIntent :: Intent
  , createResPayer :: Payer
  , createResTransactions :: [Transaction]
  , createResRedirectUrls :: Maybe RedirectUrls
  , createResPayId :: PaymentID
  , createResCreateTime :: UTCTime
  , createResPayState :: PaymentState
  , createResUpdateTime :: UTCTime
  , createResHateoasLinks :: [HateoasLink]
  } deriving (Show)

instance FromJSON CreateResponse where
  parseJSON (Object obj) =
    CreateResponse <$>
    obj .: "intent" <*>
    obj .: "payer" <*>
    obj .: "transactions" <*>
    obj .:? "redirect_urls" <*>
    obj .: "id" <*>
    (obj .: "create_time" >>= parseTimeIso8106) <*>
    obj .: "state" <*>
    (obj .: "update_time" >>= parseTimeIso8106) <*>
    obj .: "links"
  parseJSON _ = mzero

-- |Contains a parsed response from a find payment request.
data FindResponse = FindResponse
  { findResIntent :: Intent
  , findResPayer :: Payer
  , findResTransactions :: [Transaction]
  , findResRedirectUrls :: Maybe RedirectUrls
  , findResPayId :: PaymentID
  , findResCreateTime :: UTCTime
  , findResPayState :: PaymentState
  , findResUpdateTime :: UTCTime
  } deriving (Show)

instance FromJSON FindResponse where
  parseJSON (Object obj) =
    FindResponse <$>
    obj .: "intent" <*>
    obj .: "payer" <*>
    obj .: "transactions" <*>
    obj .:? "redirect_urls" <*>
    obj .: "id" <*>
    (obj .: "create_time" >>= parseTimeIso8106) <*>
    obj .: "state" <*>
    (obj .: "update_time" >>= parseTimeIso8106)
  parseJSON _ = mzero

-- |Creates a new payment using payment data.
createPayment :: CreateRequest -> PayPalOperations CreateResponse
createPayment request =
  let url = "/v1/payments/payment"
      contentType = "application/json"
      content = encode request
      payload = WTypes.Raw contentType $ HTTP.RequestBodyLBS content
  in PayPalOperation (UseHttpPost payload) url defaults

-- |Looks up a payment by ID.
findById :: PaymentID -> PayPalOperations FindResponse
findById id =
  let url = "/v1/payments/payment/" ++ id
  in PayPalOperation UseHttpGet url defaults

-- Parses a time in ISO 8106 format to a UTCTime.
parseTimeIso8106 :: String -> Parser UTCTime
parseTimeIso8106 str =
  parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ") str
