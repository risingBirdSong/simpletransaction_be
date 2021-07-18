{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Account where

import Import
import Control.Monad.Error
import Control.Exception (SomeException)



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
    
    let account = Account {
            accountName = simpleaccountName , 
            accountPassword = simpleaccountPassword ,
            accountLoggedIn = False ,
            accountBalance = 0.00 , 
            accountAdmin = False
    }
    inserted <- runDB $ insert account `catch` (\(SomeException e) -> lift $ catcher )
    sendResponseStatus status201 ("CREATED" :: String)

catcher :: HandlerFor App a
catcher = sendResponseStatus status201 ("That name has already been taken, please pick another" :: String)

postMyloginR :: Handler Value
postMyloginR = do
    attempt@Mylogin {..} <- (requireCheckJsonBody :: Handler Mylogin)
    mAcc <- runDB $ selectFirst [AccountName ==. myloginName] []
    case mAcc of 
        Nothing -> sendResponseStatus status201 ("that name doesnt exist in our records" :: String)
        Just (Entity _ user) -> do
            if (myloginPassword == accountPassword user) then do 
               _ <- runDB $ updateWhere [AccountName ==. (accountName user)] [AccountLoggedIn =. True] 
               returnJson $ object [(pack "message") .= "logged in!", (pack "loggedstatus" .= True), (pack "currentUser" .= (accountName user))]
               else returnJson $ object [(pack "message") .= "wrong password!", (pack "loggedstatus" .= False)]


postMylogoutR :: Handler Value
postMylogoutR = do 
    Mylogout {..} <- (requireCheckJsonBody :: Handler Mylogout)
    mAcc <- runDB $ selectFirst [AccountName ==. mylogoutName] []
    case mAcc of
        Nothing -> returnJson $ object [(pack "message") .= "how did you get here?"]
        Just acc -> do 
            _ <- runDB $ updateWhere [AccountName ==. mylogoutName] [AccountLoggedIn =. False]
            returnJson $ object [(pack "message") .= "logged out!", (pack "loggedstatus" .= False)]
 