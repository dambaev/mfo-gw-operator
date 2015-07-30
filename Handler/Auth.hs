{-#LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Auth where

import Import

import Yesod.Auth
import Data.Aeson
import Data.Aeson.Parser
import Data.Conduit
import Data.Conduit.Attoparsec
import Network.Wai.Conduit 
import Data.Text as T
import Data.Text.Encoding as E
import Data.ByteString.Lazy.Char8 as BL
import System.Random
import Data.List as L
import Data.Maybe
import Control.Monad
import System.Posix.PAM as PAM

-- pam service
service = "mfo-operator"

instance YesodAuth App where
    type AuthId App = Text
    
   
    getAuthId = return . Just . credsIdent
 
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [ authPAM]
    authHttpManager = httpManager

    maybeAuthId = lookupSession "gwID"


authPAM:: AuthPlugin App
authPAM = AuthPlugin "mfo" dispatch (\_ ->badMethod)
    where
    dispatch "POST" ["login"] = checkLogin >>= sendResponse
    dispatch "POST" ["verify"] = checkVerify >>= sendResponse
    dispatch _ _ = notFound

postLoginR:: AuthRoute
postLoginR = PluginR "mfo" ["login"]

getJSONRequest:: FromJSON a => HandlerT Auth (HandlerT App IO) (Result a)
getJSONRequest = do
    yesodReq <- getRequest
    let request = reqWaiRequest yesodReq
    value' <- sourceRequestBody request $$ sinkParser json
    liftIO $ print $ show value'
    return $! fromJSON value'

checkLogin:: HandlerT Auth (HandlerT App IO) Value
checkLogin = do
    deleteSession "pin"
    deleteSession "login"
    mlogin <- lookupGetParam "login"
    mpassword <- lookupGetParam "password"
    liftIO $ print $ "login,pass=" ++ (show (mlogin, mpassword))
    case (mlogin, mpassword) of
        (Just login, Just password) -> do
            eret <- lift $ PAM.authenticate service login password
            liftIO $ print $ show eret 
            case eret of
                Right _ -> do
                    pin <- liftIO $! getNewPin
                    setSession "pin" pin
                    setSession "login" login
                    sendPin "dead" pin
                    return $ -- toJSON $ OperatorNeedPin
                        object
                        [ "status" .= ("OK":: Text)
                        ]
                descr -> return $ object
                    [ "status" .= ("Error" ::Text)
                    , "comment" .= descr
                    ]
        _ -> badMethod


checkVerify:: HandlerT Auth (HandlerT App IO) Value
checkVerify = do
    mreqpin <- lookupGetParam "pin"
    mpin <- lookupSession "pin"
    case (mpin, mreqpin) of
        (Just pin, Just reqpin) | reqpin == pin -> do
            mlogin <- lookupSession "login" 
            eret <- do
                    lift $ setCreds False $ Creds "amqp" operatorUUID []
                    return $ toJSON resp
        _ -> notFound

processLoginRequest:: Result OperatorGWRequest 
                   -> HandlerT Auth (HandlerT App IO) Value
processLoginRequest (Success loginRequest@(OperatorLogin {..})) = do
    eret <- lift $ stdCall "gw.operator.session.login" loginRequest
    case eret of
        Left descr -> 
            return $ object
                [ "status" .= ("Error":: Text)
                , "comment" .= descr
                ]
        Right resp@(OperatorLoggedIn {..}) -> do
            lift $ setCreds False $ Creds "amqp" operatorUUID []
            return $ toJSON resp
        Right some ->
            return $ object
                [ "status" .= ("error":: Text)
                , "comment" .= (show some)
                ]
processLoginRequest (Error string) = do
    return $ object 
        [ "status" .= ("error":: Text)
        , "comment" .= string
        ]


getNewPin:: IO Text
getNewPin = do
    nums <- forM [1..6] $ \_ -> randomRIO (1,9)::IO Int
    let !new_pin = T.concat $ L.map (T.pack . show) $ nums
    print $ "generated pin = " ++ (show new_pin)
    return $ new_pin 



sendPin:: Text-> Text-> HandlerT Auth (HandlerT App IO) ()
sendPin mobile pin = do
    liftIO $! putStrLn $ "generated pin = " ++  show pin

