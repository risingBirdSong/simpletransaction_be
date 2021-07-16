{-# LANGUAGE RecordWildCards #-}
module Handler.Account where

import Import

-- postAccountR :: String -> Handler ()
-- postAccountR thename = do
--     -- account <- (requireCheckJsonBody :: Handler Account)
--     let account = Account {
--             accountName = ("abc") , 
--             accountPassword = "pass" ,-- bad idea don't do this!
--             accountLoggedIn = False ,
--             accountBalance = 0.00 , 
--             accountAdmin = False
--     }
--     inserted <- runDB $ insert account
--     sendResponseStatus status201 ("CREATED" :: String)

getAccountR :: String -> Handler Value 
getAccountR thename = do
    theuser <- runDB $ selectList [AccountName ==. (pack thename)] [] :: Handler [Entity Account]
    return $ object [(pack "user") .= theuser]


postSimpleaccountR :: Handler ()
postSimpleaccountR = do 
    body@Simpleaccount {..} <- (requireCheckJsonBody :: Handler Simpleaccount)
    -- name <- simpleaccountName <$> simple
    -- let name = (simpleaccountName body)
    -- problem
    Prelude.print (simpleaccountPassword)
    Couldn't match type ‘IO’ with ‘HandlerFor App’
      Expected type: HandlerFor App ()
        Actual type: IO ()
    -- solution 
    _ <- liftIO $ Prelude.print (simpleaccountName)
    _ <- liftIO $ Prelude.print (simpleaccountPassword)
    let account = Account {
            accountName = ("abc") , 
            accountPassword = "pass" ,-- bad idea don't do this!
            accountLoggedIn = False ,
            accountBalance = 0.00 , 
            accountAdmin = False
    }
    -- inserted <- runDB $ insert account
    sendResponseStatus status201 ("TODO" :: String)
