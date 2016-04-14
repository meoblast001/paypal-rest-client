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
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time.Format
import Network.Payments.PayPal
import Network.Payments.PayPal.Environment
import Network.Payments.PayPal.Payments
import Network.Payments.PayPal.Types.Currency
import Network.Payments.PayPal.Types.Payer
import Network.Payments.PayPal.Types.Transaction
import Text.Cassius
import Yesod

data Example = Example

mkYesod "Example" [parseRoutes|
/ HomeR GET
/payments PaymentsR GET
/payments/new NewPaymentR GET POST
/payments/success PaymentSuccessR GET
|]

instance Yesod Example where
  approot = ApprootStatic "http://localhost:3000"

instance RenderMessage Example FormMessage where
  renderMessage _ _ = defaultFormMessage

type UrlRenderFunction = Route Example -> T.Text

getHomeR :: Handler Html
getHomeR = redirect PaymentsR

getPaymentsR :: Handler Html
getPaymentsR = do
  listResult <- liftIO $ execPayPal sandboxUrl Config.clientId Config.secret $
                                    listPayments Nothing
  case listResult of
    Left err -> defaultLayout [whamlet|Error: #{show err}|]
    Right result ->
      let payments = listResPayments result
      in defaultLayout $(whamletFile "list_payments.hamlet")

getNewPaymentR :: Handler Html
getNewPaymentR = do
  ((ppRes, ppForm), ppEncType) <- runFormPost $ renderDivs $
                                  createPaymentFormPp Nothing
  case ppRes of
    FormSuccess result -> do
      urlRenderFunc <- getUrlRender
      createResult <- liftIO $ do
        let mayRequest = createPaymentFormPpToRequest result urlRenderFunc
        case mayRequest of
          Just request ->
            execPayPal sandboxUrl Config.clientId Config.secret $
                       createPayment request
          Nothing -> return $ Left $ OtherError "Form incorrect."
      case createResult of
        Left err -> defaultLayout [whamlet|Error: #{show err}|]
        Right result ->
          case approvalUrlFromCreate result of
            Just url ->
              redirect url
            Nothing ->
              defaultLayout [whamlet|Created Payment: #{createResPayId result}|]
    _ -> defaultLayout $(whamletFile "new_payment.hamlet")

postNewPaymentR :: Handler Html
postNewPaymentR = getNewPaymentR

data CreatePayPalPaymentForm =
  CreatePayPalPaymentForm
  { cPpPayFormItemName :: T.Text
  , cPpPayFormItemDescription :: T.Text
  , cPpPayFormItemSubtotal :: T.Text
  , cPpPayFormItemShipping :: T.Text
  , cPpPayFormItemTax :: T.Text
  } deriving (Show)

createPaymentFormPp :: Maybe CreatePayPalPaymentForm ->
                       AForm Handler CreatePayPalPaymentForm
createPaymentFormPp mayForm =
  CreatePayPalPaymentForm <$>
  areq textField "Item Name" (cPpPayFormItemName <$> mayForm) <*>
  areq textField "Item Description" (cPpPayFormItemDescription <$> mayForm) <*>
  areq textField "Item Subtotal" (cPpPayFormItemSubtotal <$> mayForm) <*>
  areq textField "Item Shipping" (cPpPayFormItemShipping <$> mayForm) <*>
  areq textField "Item Tax" (cPpPayFormItemTax <$> mayForm)

createPaymentFormPpToRequest :: CreatePayPalPaymentForm -> UrlRenderFunction ->
                                Maybe CreateRequest
createPaymentFormPpToRequest form urlRenderFunc = do
  let payer = Payer PayMethodPayPal [] Nothing Nothing
  subtotal <- realToFrac <$> fst <$>
              (eitherToMaybe $ TR.double $ cPpPayFormItemSubtotal form)
  shipping <- realToFrac <$> fst <$>
              (eitherToMaybe $ TR.double $ cPpPayFormItemShipping form)
  tax <- realToFrac <$> fst <$>
         (eitherToMaybe $ TR.double $ cPpPayFormItemTax form)
  let total = shipping + subtotal + tax
      details = Details shipping subtotal tax
      amount = Amount EUR total details
      item = Item 1 (T.unpack $ cPpPayFormItemName form) subtotal EUR "Example"
                  (Just $ T.unpack $ cPpPayFormItemDescription form)
      itemList = ItemList [item] Nothing
      transaction = Transaction amount
                    (Just $ T.unpack $ cPpPayFormItemDescription form) itemList
      redirectUrls = RedirectUrls (T.unpack $ urlRenderFunc PaymentSuccessR)
                                  (T.unpack $ urlRenderFunc HomeR)
  return $ CreateRequest SaleIntent payer [transaction] $ Just redirectUrls

getPaymentSuccessR :: Handler Html
getPaymentSuccessR = defaultLayout [whamlet|Payment successful!|]

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

main :: IO ()
main = warp 3000 Example
