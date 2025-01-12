{-# LANGUAGE OverloadedStrings #-}

module BeeGfsApi
    ( getQuota
    , setQuota
    , BeeGfsError(..)
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import BeeGfsOptions (QuotaType(..), QuotaSelection(..))

-- API Configuration
apiBaseUrl :: String
apiBaseUrl = "http://0.0.0.0:8080"  -- Point to Mountebank mock

-- Error type
data BeeGfsError = 
    ApiError Int String  -- HTTP status code and message
    | ParseError String
    deriving (Show, Eq)

instance Exception BeeGfsError

-- API Functions
getQuota :: MonadIO m => 
            Bool        -- ^ CSV output
         -> QuotaType   -- ^ Use GID or UID
         -> String      -- ^ Mount point
         -> QuotaSelection -- ^ Selection of IDs to query
         -> m (Either BeeGfsError Value)
getQuota csv quotaType mount selection = liftIO $ do
    manager <- newManager defaultManagerSettings
    
    let baseParams = 
            [ "csv=" <> if csv then "true" else "false"
            , "gid=true"
            , "mount=" <> B.pack mount
            ]
        
        selectionParams = case selection of
            Single (Just id') -> ["gid_value=" <> B.pack id']
            List ids -> ["gid_list=" <> B.pack ids]
            Range start end -> 
                [ "range_start=" <> B.pack start
                , "range_end=" <> B.pack end
                ]
            All -> []
            Single Nothing -> []

    let queryString = B.intercalate "&" $ filter (not . B.null) (baseParams ++ selectionParams)

    initReq <- parseRequest (apiBaseUrl ++ "/quota")
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

setQuota :: MonadIO m =>
            Maybe String  -- ^ GID
         -> Maybe String  -- ^ UID (always Nothing in our case)
         -> String       -- ^ Size limit
         -> Maybe String -- ^ Inode limit
         -> FilePath    -- ^ Mount point
         -> Bool        -- ^ Unlimited inodes
         -> m (Either BeeGfsError Value)
setQuota gid uid sizeLimit inodeLimit mount unlimitedInodes = liftIO $ do
    manager <- newManager defaultManagerSettings
    
    let body = encode $ object
            [ "gid" .= gid
            , "uid" .= uid
            , "size_limit" .= sizeLimit
            , "inode_limit" .= if unlimitedInodes 
                              then Just "unlimited" 
                              else inodeLimit
            , "mount" .= mount
            ]

    initReq <- parseRequest (apiBaseUrl ++ "/quota")
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