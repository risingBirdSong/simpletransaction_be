
postSimpleaccountR :: Handler ()
postSimpleaccountR = do 
    body@Simpleaccount {..} <- (requireCheckJsonBody :: Handler Simpleaccount)
    -- name <- simpleaccountName <$> simple
    -- let name = (simpleaccountName body)
    -- problem
    -- Prelude.print (simpleaccountPassword)
    -- Couldn't match type ‘IO’ with ‘HandlerFor App’
    --   Expected type: HandlerFor App ()
    --     Actual type: IO ()
    -- solution 
    _ <- liftIO $ Prelude.print (simpleaccountName)
    let account = Account {
            accountName = simpleaccountName , 
            accountPassword = simpleaccountPassword ,
            accountLoggedIn = False ,
            accountBalance = 0.00 , 
            accountAdmin = False
    }
    inserted <- runDB $ insert account
    sendResponseStatus status201 ("CREATED" :: String)
