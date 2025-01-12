{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mock (runMock) where

import Options.Applicative (execParser)
import Data.Aeson (Value(..), encode, Object)
import qualified Data.Aeson.KeyMap as KM
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

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
        Right (Object obj) -> case KM.lookup "csv" obj of
            Just (String csv) -> putStr $ T.unpack csv  -- For CSV responses
            _ -> BL.putStrLn $ encode obj          -- For JSON responses
        Right val -> BL.putStrLn $ encode val      -- For other JSON values

-- | Handle the set-quota command
handleSetQuota :: SetQuotaOpts -> IO ()
handleSetQuota SetQuotaOpts{..} = do
    result <- Api.setQuota 
        sqGid
        sqSizeLimit
        sqInodeLimit
        sqMount
        sqUnlimitedInodes
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right val -> BL.putStrLn $ encode val