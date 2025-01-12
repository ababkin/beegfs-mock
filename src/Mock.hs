{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mock (runMock) where

import Options.Applicative (execParser)
import Data.Aeson (Value)
import Control.Monad.IO.Class (liftIO)

import BeeGfsOptions
    ( BeeGfsCommand(..)
    , GetQuotaOpts(..)
    , SetQuotaOpts(..)
    , opts
    )
import qualified BeeGfsApi as Api

-- | Main entry point for the mock BeeGFS CLI
runMock :: IO ()
runMock = execParser opts >>= handleCommand

-- | Handle the different CLI commands
handleCommand :: BeeGfsCommand -> IO ()
handleCommand (GetQuota gopts) = handleGetQuota gopts
handleCommand (SetQuota sopts) = handleSetQuota sopts

-- | Handle the get-quota command
handleGetQuota :: GetQuotaOpts -> IO ()
handleGetQuota GetQuotaOpts{..} = do
    result <- Api.getQuota 
        gqCsv
        gqType
        gqMount
        gqSelection
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right response -> putStrLn $ "Response: " ++ show response

-- | Handle the set-quota command
handleSetQuota :: SetQuotaOpts -> IO ()
handleSetQuota SetQuotaOpts{..} = do
    result <- Api.setQuota 
        (Just sqGid)
        Nothing
        sqSizeLimit
        (Just sqInodeLimit)
        sqMount
        sqUnlimitedInodes
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right response -> putStrLn $ "Response: " ++ show response