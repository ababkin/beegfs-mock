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
    
    let queryString = "quota=userquota@" <> B.pack username

    initReq <- parseRequest (apiBaseUrl ++ "/zfs/quota")
    let request = initReq
            { method = "GET"
            , queryString = queryString
            }

    response <- httpLbs request manager
    let status = responseStatus response
    
    pure $ if statusCode status >= 200 && statusCode status < 300
        then case eitherDecode (responseBody response) of
            Left err -> Left $ ParseError err
            Right val -> Right val
        else Left $ ApiError (statusCode status) (show $ responseBody response)

-- | Set storage quota for a user
setStorageQuota :: MonadIO m =>
                   String   -- ^ Username
                -> String   -- ^ Quota size (e.g., "100G")
                -> m (Either ZfsError Value)
setStorageQuota username size = liftIO $ do
    manager <- newManager defaultManagerSettings
    
    let body = encode $ object
            [ "username" .= username
            , "size" .= size
            ]

    initReq <- parseRequest (apiBaseUrl ++ "/zfs/storage-quota")
    let request = initReq
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = 
                [ ("Content-Type", "application/json")
                ]
            }

    response <- httpLbs request manager
    let status = responseStatus response
    pure $ if statusCode status >= 200 && statusCode status < 300
        then case eitherDecode (responseBody response) of
            Left err -> Left $ ParseError err
            Right val -> Right val
        else Left $ ApiError (statusCode status) (show $ responseBody response)

-- | Set object quota for a user
setObjectQuota :: MonadIO m =>
                  String    -- ^ Username
               -> String    -- ^ Object count
               -> m (Either ZfsError Value)
setObjectQuota username count = liftIO $ do
    manager <- newManager defaultManagerSettings
    
    let body = encode $ object
            [ "username" .= username
            , "count" .= count
            ]

    initReq <- parseRequest (apiBaseUrl ++ "/zfs/object-quota")
    let request = initReq
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = 
                [ ("Content-Type", "application/json")
                ]
            }

    response <- httpLbs request manager
    let status = responseStatus response
    pure $ if statusCode status >= 200 && statusCode status < 300
        then case eitherDecode (responseBody response) of
            Left err -> Left $ ParseError err
            Right val -> Right val
        else Left $ ApiError (statusCode status) (show $ responseBody response)
