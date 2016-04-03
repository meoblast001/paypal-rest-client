-- |
-- Module: Network.Payments.PayPal.Types.Hateoas
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Types.Hateoas (HateoasLink(..)) where

import Control.Monad
import qualified Data.Text as T
import Data.Aeson
import Network.Payments.PayPal

type URL = String

data HateoasLink = HateoasLink
  { hateoasHref :: URL
  , hateoasRel :: String
  , hateoasMethod :: Either HttpMethod String
  } deriving (Eq, Show)

instance FromJSON HateoasLink where
  parseJSON (Object obj) =
    HateoasLink <$>
    obj .: "href" <*>
    obj .: "rel" <*>
    ((obj .: "method") >>= parseHttpMethod)
    where
      parseHttpMethod (String "GET") = return $ Left HttpGet
      parseHttpMethod (String "POST") = return $ Left HttpPost
      parseHttpMethod (String other) = return $ Right $ T.unpack other
      parseHttpMethod _ = mzero
  parseJSON _ = mzero
