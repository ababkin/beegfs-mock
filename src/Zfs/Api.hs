{-# LANGUAGE OverloadedStrings #-}

module Zfs.Api
    ( getQuota
    , setStorageQuota
    , setObjectQuota
    , ZfsError(..)
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL

-- API Configuration
apiBaseUrl :: String
apiBaseUrl = "http://0.0.0.0:8085"  -- Point to Mountebank mock

-- Error type
data ZfsError = 
    ApiError Int String  -- HTTP status code and message
    | ParseError String
    deriving (Show, Eq)

instance Exception ZfsError

-- | Get quota for a user
getQuota :: MonadIO m => 
            String    -- ^ Username to query
         -> m (Either ZfsError Value)
getQuota username = liftIO $ do
    manager <- newManager defaultManagerSettings
    
    initReq <- parseRequest $ apiBaseUrl ++ "/zfs/userquota/" ++ username
    let request = initReq
            { method = "GET"
            , requestHeaders = 
                [ ("Content-Type", "application/json")
                ]
            }

    response <- httpLbs request manager
    let status = statusCode $ responseStatus response
    if status == 200
        then case eitherDecode (responseBody response) of
            Left err -> return $ Left $ ParseError err
            Right val -> return $ Right val
        else return $ Left $ ApiError status (show $ responseBody response)

-- | Set storage quota for a user
setStorageQuota :: MonadIO m =>
                   String    -- ^ Username
                -> String    -- ^ Size
                -> m (Either ZfsError Value)
setStorageQuota username size = liftIO $ do
    manager <- newManager defaultManagerSettings
    
    let body = encode $ object
            [ "username" .= username
            , "size" .= size
            ]

    initReq <- parseRequest $ apiBaseUrl ++ "/zfs/userquota"
    let request = initReq
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = 
                [ ("Content-Type", "application/json")
                ]
            }

    response <- httpLbs request manager
    let status = statusCode $ responseStatus response
    if status == 200
        then case eitherDecode (responseBody response) of
            Left err -> return $ Left $ ParseError err
            Right val -> return $ Right val
        else return $ Left $ ApiError status (show $ responseBody response)

-- | Set object quota for a user
setObjectQuota :: MonadIO m =>
                  String    -- ^ Username
               -> String    -- ^ Count
               -> m (Either ZfsError Value)
setObjectQuota username count = liftIO $ do
    manager <- newManager defaultManagerSettings
    
    let body = encode $ object
            [ "username" .= username
            , "count" .= count
            ]

    initReq <- parseRequest $ apiBaseUrl ++ "/zfs/userobjquota"
    let request = initReq
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = 
                [ ("Content-Type", "application/json")
                ]
            }

    response <- httpLbs request manager
    let status = statusCode $ responseStatus response
    if status == 200
        then case eitherDecode (responseBody response) of
            Left err -> return $ Left $ ParseError err
            Right val -> return $ Right val
        else return $ Left $ ApiError status (show $ responseBody response)
