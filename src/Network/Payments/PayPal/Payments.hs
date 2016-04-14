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
, RedirectUrls(..)
, ReturnLinkParams(..)
, CreateRequest(..)
, CreateResponse(..)
, ExecuteRequest(..)
, ExecuteResponse(..)
, FindResponse(..)
, ListResponse(..)
, createPayment
, approvalUrlFromCreate
, executePayment
, findPaymentById
, listPayments
, returnLinkParams
) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import qualified Network.HTTP.Client as HTTP
import Network.Payments.PayPal
import Network.Payments.PayPal.Types.Hateoas
import Network.Payments.PayPal.Types.Paging
import Network.Payments.PayPal.Types.Payer
import Network.Payments.PayPal.Types.Transaction
import Network.Wreq
import qualified Network.Wreq.Types as WTypes

-- A string representing a URL.
type URL = String

-- The ID of a payment provided by PayPal.
type PaymentID = String

-- |Payment intent.
data Intent = SaleIntent | AuthoriseIntent | OrderIntent
  deriving (Eq, Read, Show)

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
  } deriving (Eq, Show)

instance ToJSON RedirectUrls where
  toJSON urls =
    object ["return_url" .= redirUrlReturn urls,
            "cancel_url" .= redirUrlCancel urls]

instance FromJSON RedirectUrls where
  parseJSON (Object obj) =
    RedirectUrls <$>
    obj .: "return_url" <*>
    obj .: "cancel_url"
  parseJSON _ = mzero

data ReturnLinkParams = ReturnLinkParams
  { retLinkParamPayId :: PaymentID
  , retLinkParamToken :: String
  , retLinkParamPayerId :: String
  } deriving (Eq, Show)

-- |Current status of the PayPal payment.
data PaymentState = PayStateCreated | PayStateApproved | PayStateFailed |
                    PayStateCancelled | PayStateExpired | PayStatePending
                    deriving (Eq, Read, Show)

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
  , createReqRedirectUrls :: Maybe RedirectUrls
  } deriving (Eq, Show)

instance ToJSON CreateRequest where
  toJSON req =
    object (["intent" .= createReqIntent req,
             "payer" .= createReqPayer req,
             "transactions" .= createReqTransactions req] ++
            maybeToList (("redirect_urls" .=) <$> createReqRedirectUrls req))

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
  , createResUpdateTime :: Maybe UTCTime
  , createResHateoasLinks :: [HateoasLink]
  } deriving (Eq, Show)

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
    (obj .:? "update_time" >>=
     maybe (return Nothing) (\str -> Just <$> parseTimeIso8106 str)) <*>
    obj .: "links"
  parseJSON _ = mzero

-- |Executing a payment has a special transaction object which only contains the
-- amount.
data ExecuteTransaction = ExecuteTransaction
  { executeTransactionAmount :: Amount
  } deriving (Eq, Show)

instance ToJSON ExecuteTransaction where
  toJSON trans = object ["amount" .= executeTransactionAmount trans]

-- |Request to execute a payment.
data ExecuteRequest = ExecuteRequest
  { executeReqPayerId :: String
  , executeReqTransactions :: [ExecuteTransaction]
  } deriving (Eq, Show)

instance ToJSON ExecuteRequest where
  toJSON req =
    object ["payer_id" .= executeReqPayerId req,
            "transactions" .= executeReqTransactions req]

-- |Response from an execute payment request.
data ExecuteResponse = ExecuteResponse
  { executeResIntent :: Intent
  , executeResPayer :: Payer
  , executeResTransactions :: [Transaction]
  , executeResHateoasLinks :: [HateoasLink]
  } deriving (Eq, Show)

instance FromJSON ExecuteResponse where
  parseJSON (Object obj) =
    ExecuteResponse <$>
    obj .: "intent" <*>
    obj .: "payer" <*>
    obj .: "transactions" <*>
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
  , findResUpdateTime :: Maybe UTCTime
  } deriving (Eq, Show)

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
    (obj .:? "update_time" >>= maybe (return Nothing)
                                     (\str -> Just <$> parseTimeIso8106 str))
  parseJSON _ = mzero

-- Response to a payment list request.
data ListResponse = ListResponse
  { listResPayments :: [CreateResponse]
  , listResCount :: Integer
  , listResNextId :: Maybe PaymentID
  } deriving (Eq, Show)

instance FromJSON ListResponse where
  parseJSON (Object obj) =
    ListResponse <$>
    (fromMaybe [] <$> obj .:? "payments") <*>
    obj .: "count" <*>
    obj .:? "next_id"
  parseJSON _ = mzero

-- |Creates a new payment using payment data.
createPayment :: CreateRequest -> PayPalOperations CreateResponse
createPayment request =
  let url = "/v1/payments/payment"
      contentType = "application/json"
      content = encode request
      payload = WTypes.Raw contentType $ HTTP.RequestBodyLBS content
  in PayPalOperation (UseHttpPost payload) url defaults

-- |Extracts an approval URL, if there is one, from a create response
approvalUrlFromCreate :: CreateResponse -> Maybe URL
approvalUrlFromCreate response =
  let criteriaFunction hateoas =
        hateoasRel hateoas == "approval_url" &&
        hateoasMethod hateoas == HateoasRedirect
      maybeHateoas =
        listToMaybe $ filter criteriaFunction $ createResHateoasLinks response
  in hateoasHref <$> maybeHateoas

-- |Execute (or complete) a payment that has been approved by the payer.
executePayment :: PaymentID -> ExecuteRequest ->
                  PayPalOperations ExecuteResponse
executePayment id' request =
  let url = "/v1/payments/payment/" ++ id' ++ "/execute"
      contentType = "application/json"
      content = encode request
      payload = WTypes.Raw contentType $ HTTP.RequestBodyLBS content
  in PayPalOperation (UseHttpPost payload) url defaults

-- |Looks up a payment by ID.
findPaymentById :: PaymentID -> PayPalOperations FindResponse
findPaymentById id' =
  let url = "/v1/payments/payment/" ++ id'
  in PayPalOperation UseHttpGet url defaults

-- |Lists payments, possibly with paging.
listPayments :: Maybe PagingRequest -> PayPalOperations ListResponse
listPayments pagingRequest =
  let url = "/v1/payments/payment/" ++
            (maybe mempty (\req -> "?" ++ pagingReqToQuery req) pagingRequest)
  in PayPalOperation UseHttpGet url defaults

-- |Use this function to parse GET parameters passed from PayPal to the return
-- URL. Takes a map of query parameters (name to value) and returns a structure
-- containing these parameters if possible. Otherwise PayPal didn't give you
-- required data.
returnLinkParams :: M.Map BS.ByteString BS.ByteString -> Maybe ReturnLinkParams
returnLinkParams query = do
  paymentId <- BS8.unpack <$> M.lookup "paymentId" query
  token <- BS8.unpack <$> M.lookup "token" query
  payerId <- BS8.unpack <$> M.lookup "PayerID" query
  return $ ReturnLinkParams paymentId token payerId

-- Parses a time in ISO 8106 format to a UTCTime.
parseTimeIso8106 :: String -> Parser UTCTime
parseTimeIso8106 str =
  parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ") str
