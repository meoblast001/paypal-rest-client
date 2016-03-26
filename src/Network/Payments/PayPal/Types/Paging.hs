-- |
-- Module: Network.Payments.PayPal.Types.Paging
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Types.Paging
( PagingSortBy(..)
, PagingSortOrder(..)
, PagingRequest(..)
, pagingReqToQuery
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Time.Clock
import Data.Time.Format
import Network.HTTP.Types.URI

-- |Sort by create or update time.
data PagingSortBy = PagingSortCreated | PagingSortUpdated

instance Show PagingSortBy where
  show PagingSortCreated = "create_time"
  show PagingSortUpdated = "update_time"

-- |Sort order.
data PagingSortOrder = PagingSortAsc | PagingSortDesc

instance Show PagingSortOrder where
  show PagingSortAsc = "asc"
  show PagingSortDesc = "desc"

-- |Request query parameters to page lists of data in a response.
data PagingRequest = PagingRequest
  { pagingCount :: Maybe Integer
  , pagingStartId :: Maybe String
  , pagingStartTime :: UTCTime
  , pagingEndTime :: UTCTime
  , pagingSortBy :: Maybe PagingSortBy
  , pagingSortOrder :: Maybe PagingSortOrder
  } deriving (Show)

-- |Create a query string from a paging request.
pagingReqToQuery :: PagingRequest -> String
pagingReqToQuery req =
  let queryItems = [("count", showToBS <$> pagingCount req),
                    ("start_id", stringToBS <$> pagingStartId req),
                    ("start_time", Just $ timeToBS $ pagingStartTime req),
                    ("end_time", Just $ timeToBS $ pagingEndTime req),
                    ("sort_by", showToBS <$> pagingSortBy req),
                    ("sort_order", showToBS <$> pagingSortOrder req)]
  in BS8.unpack $ renderQuery False queryItems
  where
    stringToBS :: String -> BS.ByteString
    stringToBS x = BS8.pack x
    timeToBS :: UTCTime -> BS.ByteString
    timeToBS x = BS8.pack $ formatTime defaultTimeLocale
                                       (iso8601DateFormat $ Just "%H:%M:%SZ") x
    showToBS :: Show a => a -> BS.ByteString
    showToBS x = BS8.pack $ show x
