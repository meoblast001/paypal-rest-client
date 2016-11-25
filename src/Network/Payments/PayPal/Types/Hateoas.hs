-- |
-- Module: Network.Payments.PayPal.Types.Hateoas
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Payments.PayPal.Types.Hateoas
( HateoasMethod(..)
, HateoasLink(..)
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import qualified Data.Text as T
import Data.Aeson

type URL = String

-- |HATEOAS method.
data HateoasMethod = HateoasGet | HateoasPost | HateoasPatch | HateoasRedirect |
                     HateoasOther String deriving (Eq, Read, Show)

data HateoasLink = HateoasLink
  { hateoasHref :: URL
  , hateoasRel :: String
  , hateoasMethod :: HateoasMethod
  } deriving (Eq, Show)

instance FromJSON HateoasLink where
  parseJSON (Object obj) =
    HateoasLink <$>
    obj .: "href" <*>
    obj .: "rel" <*>
    ((obj .: "method") >>= parseHttpMethod)
    where
      parseHttpMethod (String "GET") = return HateoasGet
      parseHttpMethod (String "POST") = return HateoasPost
      parseHttpMethod (String "PATCH") = return HateoasPatch
      parseHttpMethod (String "REDIRECT") = return HateoasRedirect
      parseHttpMethod (String other) = return $ HateoasOther $ T.unpack other
      parseHttpMethod _ = mzero
  parseJSON _ = mzero
