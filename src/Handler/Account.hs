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


-- is there a better way to do this? Kinda feels hacky using Simpleaccount to send name and password just so they can be placed in an Account. 
-- The reason for this is because default wasnt working and I didnt want the FE to send all that excess data.

postSimpleaccountR :: Handler ()
postSimpleaccountR = do 
    Simpleaccount {..} <- (requireCheckJsonBody :: Handler Simpleaccount)
    doesexist <- runDB $ selectFirst [AccountName ==. simpleaccountName] []
    case doesexist of
        Just alreadyName -> sendResponseStatus status201 ("That name is already taken, choose another" :: String)
        Nothing -> do 
            let account = Account {
                    accountName = simpleaccountName , 
                    accountPassword = simpleaccountPassword ,
                    accountLoggedIn = False ,
                    accountBalance = 0.00 , 
                    accountAdmin = False
            }
            inserted <- runDB $ insert account
            sendResponseStatus status201 ("CREATED" :: String)
