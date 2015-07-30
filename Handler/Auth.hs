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
import System.Process
import System.Exit

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
    mlogin <- lookupPostParam "login"
    mpassword <- lookupPostParam "password"
    liftIO $ print $ "login,pass=" ++ (show (mlogin, mpassword))
    case (mlogin, mpassword) of
        (Just login, Just password) -> do
            eret <- liftIO $ PAM.authenticate service (T.unpack login) (T.unpack password)
            liftIO $ print $ show eret 
            case eret of
                Right _ -> do
                    etel <- liftIO $! getUserTelephone login
                    case etel of
                         Left reason -> do
                             liftIO $ print reason
                             badMethod
                         Right tel -> do
                             pin <- liftIO $! getNewPin
                             setSession "pin" pin
                             setSession "login" login
                             sendPin tel pin
                             return $ -- toJSON $ OperatorNeedPin
                                object
                                [ "status" .= ("OK":: Text)
                                ]
                Left descr -> badMethod
        _ -> badMethod


checkVerify:: HandlerT Auth (HandlerT App IO) Value
checkVerify = do
    mreqpin <- lookupPostParam "pin"
    mpin <- lookupSession "pin"
    liftIO $ print $ "(mpin,mpib) = " ++ show (mreqpin, mpin)
    sess <- getSession
    liftIO $ print $ sess
    case (mpin, mreqpin) of
        (Just pin, Just reqpin) | reqpin == pin -> do
            mlogin <- lookupSession "login" 
            case mlogin of
                Nothing -> notFound
                Just login -> do
                lift $ setCreds False $ Creds "mfo" login []
                return $ object
                    [ "status" .= ("OK":: Text)
                    ]
        _ -> badMethod

{- processLoginRequest:: Result OperatorGWRequest 
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
            lift $ setCreds False $ Creds "mfo" operatorUUID []
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

        -} 

getNewPin:: IO Text
getNewPin = do
    nums <- forM [1..6] $ \_ -> randomRIO (1,9)::IO Int
    let !new_pin = T.concat $ L.map (T.pack . show) $ nums
    print $ "generated pin = " ++ (show new_pin)
    return $ new_pin 



sendPin:: Text-> Text-> HandlerT Auth (HandlerT App IO) ()
sendPin mobile pin = do
    liftIO $! do
        Import.putStrLn $ "generated pin = " ++  show pin
        Import.putStrLn $ "for telephone = " ++  show mobile

getUserTelephone:: Text-> IO (Either Text Text)
getUserTelephone login = do
    (code, stdout, stderr) <- readProcessWithExitCode 
        ("getent")
        ["passwd", (T.unpack login)]
        ""
    case code of
         _ | code /= ExitSuccess || L.length stdout < 1-> return $ Left $ T.pack stderr
         _ -> do
             let splitted = T.splitOn "," $! (T.pack stdout)
             if L.length splitted < 3
                then return $ Left "no telephone"
                else return $ Right $! splitted !! 2
