{-# LANGUAGE OverloadedStrings #-}

module BeeGfsApi
    ( getQuota
    , setQuota
    , BeeGfsError(..)
    ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T

-- API Configuration
apiBaseUrl :: String
apiBaseUrl = "http://beegfs-api:8080/v1"  -- Example URL

-- Error type
data BeeGfsError = 
    ApiError Int String  -- HTTP status code and message
    | ParseError String
    deriving (Show, Eq)

instance Exception BeeGfsError

-- API Functions
getQuota :: MonadIO m => 
            Bool     -- ^ CSV output
         -> Bool     -- ^ Use GID
         -> Bool     -- ^ Use UID
         -> String   -- ^ Mount point
         -> Maybe String  -- ^ User list
         -> Maybe String  -- ^ UID value
         -> Maybe String  -- ^ GID value
         -> m (Either BeeGfsError Value)
getQuota csv useGid useUid mount userList uidVal gidVal = liftIO $ do
    let queryParams = 
            [ ("csv", Just $ if csv then "true" else "false")
            , ("gid", Just $ if useGid then "true" else "false")
            , ("uid", Just $ if useUid then "true" else "false")
            , ("mount", Just $ B.pack mount)
            ] ++ 
            maybe [] (\l -> [("list", Just $ B.pack l)]) userList ++
            maybe [] (\u -> [("uid_value", Just $ B.pack u)]) uidVal ++
            maybe [] (\g -> [("gid_value", Just $ B.pack g)]) gidVal

    request <- setRequestQueryString queryParams
             . setRequestMethod "GET"
             . setRequestPath "/quota"
             $ defaultRequest { host = "beegfs-api"
                            , port = 8080 }

    handleResponse <$> httpJSON request

setQuota :: MonadIO m =>
            Maybe String  -- ^ GID
         -> Maybe String  -- ^ UID
         -> String       -- ^ Size limit
         -> Maybe String -- ^ Inode limit
         -> String      -- ^ Mount point
         -> Bool        -- ^ Unlimited inodes
         -> m (Either BeeGfsError Value)
setQuota gid uid sizeLimit inodeLimit mount unlimitedInodes = liftIO $ do
    let body = object
            [ "gid" .= gid
            , "uid" .= uid
            , "size_limit" .= sizeLimit
            , "inode_limit" .= if unlimitedInodes 
                              then Just "unlimited" 
                              else inodeLimit
            , "mount" .= mount
            ]

    request <- setRequestBodyJSON body
             . setRequestMethod "POST"
             . setRequestPath "/quota"
             $ defaultRequest { host = "beegfs-api"
                            , port = 8080 }

    handleResponse <$> httpJSON request

-- Helper function to handle API responses
handleResponse :: Response Value -> Either BeeGfsError Value
handleResponse response =
    let status = getResponseStatusCode response
    in if status >= 200 && status < 300
       then Right $ getResponseBody response
       else Left $ ApiError status (show $ getResponseBody response) 