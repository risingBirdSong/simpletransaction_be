{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE RecordWildCards #-}
module Handler.Account where

import Import
import Control.Monad.Error
import Control.Exception (SomeException)

safeAccountInfo :: Account -> Handler Value
safeAccountInfo (Account {..}) = do 
    returnJson $ object [
        (pack "name") .= accountName , 
        (pack "loggedIn" .= accountLoggedIn) ,
        (pack "balance") .= accountBalance
        ]

getAccountR :: String -> Handler Value 
getAccountR thename = do
    (mAccount) <- runDB $ selectFirst [AccountName ==. (pack thename)] []
    case mAccount of
        Just (Entity _ account) -> safeAccountInfo account 
        Nothing -> sendResponseStatus status404 ("The account not found" :: String)

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
 

data TransactionParties = TransactionParties {
    from :: String , 
    to   :: String
} deriving (Generic, Show)

instance FromJSON TransactionParties


postTransactionR :: Handler Value
postTransactionR = do
    aaa@(Transaction {..}) <- (requireCheckJsonBody) `catch` (\(SomeException e) -> sendResponseStatus status201 ((show e) :: String)) -- HCError InvalidArgs ["key \"id\" not found"] 
    -- printing aaa ===> Transaction {transactionNote = "pay a", transactionAmount = 0.1, transactionFrom = "aaa", transactionTo = "bbb"}
    mFrom <- runDB $ selectFirst [AccountName ==. (transactionFrom)] []
    mTo <- runDB $ selectFirst [AccountName ==. (transactionTo )] []
    case (mFrom, mTo) of
        (Just (Entity fromkey from) , Just (Entity tokey to))
            | (isNaN (transactionAmount)) -> sendResponseStatus status201 ("we couldnt read the number" :: String)
            | ((transactionAmount) > (accountBalance from)) -> sendResponseStatus status201 ("insufficient funds" :: String) 
            | otherwise -> do
                _ <- runDB $ updateWhere [AccountName ==. (accountName from)] [AccountBalance -=. (transactionAmount)] 
                _ <- runDB $ updateWhere [AccountName ==. (accountName to)] [AccountBalance +=. (transactionAmount)]
                mEntity <- runDB $ selectFirst [AccountName ==. (accountName from)] []
                _ <- runDB $ insert $ Transaction {..}
                case mEntity of
                    Just (Entity _ newFrom) -> sendResponseStatus status201 ("you successfully sent " ++ (show $ transactionAmount) ++ " to " ++ (show $ accountName to) ++ ". Your new balance is " ++ (show (accountBalance newFrom)) :: String)
                    Nothing -> sendResponseStatus status201 ("wait... what just happened?" :: String)
        
        _ -> do 
                
                sendResponseStatus status201 ("one of the accounts didnt work" :: String)
    returnJson $ object [(pack "res") .= "test"]

getTransactiongetR :: String -> Handler Value
getTransactiongetR username = do 
    eAccounts <- runDB $ selectList [AccountName !=. username] [Asc AccountName]
    let accounts  = map (\(Entity _ acc) -> acc) eAccounts
    let displayaccounts = map displayAccount accounts
    returnJson $ object $ [(pack "accounts") .= displayaccounts]


data PublicAccount = PublicAccount {
    publicName :: String
} deriving (Generic, Show)

instance FromJSON PublicAccount
instance ToJSON PublicAccount

displayAccount :: Account -> PublicAccount
displayAccount accnt = PublicAccount {
        publicName = accountName accnt
    }
