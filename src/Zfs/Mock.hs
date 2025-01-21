{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Zfs.Mock (runMock) where

import Options.Applicative (execParser)
import Data.Aeson (Value(..), encode, Object)
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import Zfs.Options
    ( ZfsCommand(..)
    , parseZfsOpts
    )
import qualified Zfs.Api as Api

-- | Main entry point for the mock ZFS CLI
runMock :: IO ()
runMock = parseZfsOpts >>= handleCommand

-- | Handle the different CLI commands
handleCommand :: ZfsCommand -> IO ()
handleCommand (GetQuota username) = handleGetQuota username
handleCommand (SetStorageQuota username size) = handleSetStorageQuota username size
handleCommand (SetObjectQuota username count) = handleSetObjectQuota username count

-- | Handle the get-quota command
handleGetQuota :: String -> IO ()
handleGetQuota username = do
    result <- Api.getQuota username
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right val -> formatZfsQuota username val

-- | Format quota output in ZFS style
formatZfsQuota :: String -> Value -> IO ()
formatZfsQuota username (Object obj) = 
    putStrLn $ "home1\tuserquota@" ++ username ++ "\t" ++ quotaValue ++ "\t-"
  where
    quotaValue = case KeyMap.lookup "quota" obj of
        Just (String quota) -> T.unpack quota
        _ -> "0"
formatZfsQuota _ _ = putStrLn "Error: Invalid quota format"

-- | Handle the set-storage-quota command
handleSetStorageQuota :: String -> String -> IO ()
handleSetStorageQuota username size = do
    result <- Api.setStorageQuota username size
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right val -> BL.putStrLn $ encode val

-- | Handle the set-object-quota command
handleSetObjectQuota :: String -> String -> IO ()
handleSetObjectQuota username count = do
    result <- Api.setObjectQuota username count
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right val -> BL.putStrLn $ encode val
