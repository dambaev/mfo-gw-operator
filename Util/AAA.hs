{-#LANGUAGE BangPatterns #-}
{-#LANGUAGE OverloadedStrings #-}
module Util.AAA 
    ( getUserGroups
    ) where

import Yesod
import Prelude
import System.Process
import System.Exit
import Data.List as DL
import Data.Text as T
import Data.ByteString.UTF8 as DBU8


{-| gets list of user's groups
 -
 -}
getUserGroups:: Text-> IO (Either Text [Text])
getUserGroups login = do
    (code, !stdout, stderr) <- readProcessWithExitCode 
        "id"
        [T.unpack login]
        ""
    case code of
        _ | code /= ExitSuccess || DL.length stdout < 1 -> do
            return $ Left (T.pack stderr)
        _ -> do
            let !splitted = T.splitOn "=" $! T.pack stdout
                groups = splitted !! 3
            if DL.length splitted < 4 then return $ Left "no groups"
                else do
                    let sgroups = T.splitOn "," groups
                        remap group = T.takeWhile (/=')') $! T.drop 1 $! 
                            T.dropWhile (/= '(') group
                        !rgroups = DL.map remap sgroups
                    print $ "user " ++ show login ++ " is in groups " ++ show rgroups
                    return $! Right rgroups
