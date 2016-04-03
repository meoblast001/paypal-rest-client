-- |
-- Module: Main
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Config as Config
import Data.Time.Format
import Network.Payments.PayPal
import Network.Payments.PayPal.Environment
import Network.Payments.PayPal.Payments
import Yesod

data Example = Example

mkYesod "Example" [parseRoutes|
/ HomeR GET
/payments/list PaymentListR GET
|]

instance Yesod Example

getHomeR :: Handler Html
getHomeR = redirect PaymentListR

getPaymentListR :: Handler Html
getPaymentListR = do
  listResult <- liftIO $ execPayPal sandboxUrl Config.clientId Config.secret $
                                    listPayments Nothing
  case listResult of
    Left err -> defaultLayout [whamlet|Error: #{show err}|]
    Right listResultSuccess ->
      let payments = listResPayments listResultSuccess
      in defaultLayout $(whamletFile "list_payments.hamlet")

main :: IO ()
main = warp 3000 Example
