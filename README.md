### PayPal Client (for Haskell) ###

This library allows you to connect with PayPal's
[REST API V1](https://developer.paypal.com/docs/api/) through Haskell.

###### The PayPal Monad and Session

PayPal's API uses OAuth 2 and therefore provides access tokens which expire.
When this happens, the access token must be renewed. This problem is solved by
the `PayPalOperations` monad. This monad can chain PayPal actions together which
will be executed consecutively by `execPayPal`. If the token expires before an
action is executed, the token is first renewed.

Suppose you need to create a new payment, then get that payment ID back as well
as a list of all payment IDs. You could write this:

    import Network.Payments.PayPal
    import Network.Payments.PayPal.Payments

    myOperations :: PayPalOperations (PaymentID, [PaymentID])
    myOperations = do
      cRes <- createPayment $ CreateResponse ...
      lRes <- listPayments Nothing
      return (createResPayId cRes, map createResPayId $ listResPayments lRes)

Then, given you have a `Config` module with the credentials, you can execute the
monad and get your results like so

    import Config
    import Network.Payments.PayPal

    main :: IO ()
    main = do
      payPalResult <- execPayPal sandboxUrl Config.clientId Config.secret
                                 myOperations
      case payPalResult of
        Left err -> show err
        Right (newId, allIds) -> do
          putStrLn ("The new ID is: " ++ show newId)
          putStrLn ("All IDs are: " ++ show allIds)

The monad does not need to take error handling into account. If an error occurs,
the monad receives a short circuit and the rest of the code is not executed. The
error is simply returned to be handled. In the example, this means if
`createPayment` fails, `listPayments` will never run.
