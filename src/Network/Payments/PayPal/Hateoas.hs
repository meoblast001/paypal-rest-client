-- |
-- Module: Network.Payments.PayPal.Hateoas
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Hateoas (HateoasLink(..)) where

import Control.Monad
import Data.Aeson
import Network.Payments.PayPal

type URL = String

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
