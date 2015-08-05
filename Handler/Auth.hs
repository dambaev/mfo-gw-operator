{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Auth where

-- import Import

import Yesod
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
import Prelude as P
import Codec.MFO.API.Operator
import Codec.JSON.RPC
import Data.ByteString.UTF8 as BU8

-- pam service
service = "mfo-operator"

authPAM:: AuthPlugin site
authPAM = AuthPlugin "mfo" dispatch (\_ ->badMethod)
    where
    dispatch "POST" ["login"] = checkLogin >>= sendResponse
    dispatch "POST" ["verify"] = checkVerify >>= sendResponse
    dispatch _ _ = notFound

postLoginR:: AuthRoute
postLoginR = PluginR "mfo" ["login"]

checkLogin:: HandlerT Auth (HandlerT site IO) Value
checkLogin = do
    clearSession
    req <- getJSONRequest
    case req of
        Error reason -> return $! toJSON $! OAAError $! E.decodeUtf8 $! 
             BU8.fromString reason
        Success (ORALogin {..}) -> do
            liftIO $ print $ "login,pass=" ++ (show (oraLogin, oraPassword))
            eret <- liftIO $ PAM.authenticate service (T.unpack oraLogin) (T.unpack oraPassword)
            liftIO $ print $ show eret 
            case eret of
                Right _ -> do
                    etel <- liftIO $! getUserTelephone oraLogin
                    case etel of
                         Left reason -> do
                             liftIO $ print reason
                             return $! toJSON $! OAAError reason
                         Right (fname, tel) -> do
                             pin <- liftIO $! getNewPin
                             setSession "pin" pin
                             setSession "login" oraLogin
                             setSession "fullname" fname
                             sendPin tel pin
                             return $ toJSON $ OAANeedPin
                Left descr -> badMethod
        _ -> return $! toJSON $! OAAError "bad request"


checkVerify:: YesodAuth site => HandlerT Auth (HandlerT site IO) Value
checkVerify = do
    mpin <- lookupSession "pin"
    req <- getJSONRequest
    case (req,mpin) of
        (Error reason, _)-> return $! toJSON $! OAAError $! E.decodeUtf8 $!
             BU8.fromString reason
        (_, Nothing)-> return $! toJSON $! OAAError $! "need pin"
        (Success ORAPin {..}, Just pin) | oraPin == pin -> do
            liftIO $ print $ "(mreqpin,mpin) = " ++ show (oraPin, pin)
            mlogin <- lookupSession "login" 
            Just fname <- lookupSession "fullname"
            case mlogin of
                Nothing -> return $! toJSON $! OAAError $! "corrupt session"
                Just login -> do
                lift $ setCreds False $ Creds "mfo" login []
                return $! toJSON $! OAALoggedIn fname
        _ -> return $! toJSON $! OAAError $! "bad request"
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



sendPin:: Text-> Text-> HandlerT Auth (HandlerT site IO) ()
sendPin mobile pin = do
    liftIO $! do
        P.putStrLn $ "generated pin = " ++  show pin
        P.putStrLn $ "for telephone = " ++  show mobile

getUserTelephone:: Text-> IO (Either Text (Text, Text))
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
                else return $ Right $! (splitted !! 0, splitted !! 2)
