{-# LANGUAGE RecordWildCards #-}

module BeeGfsOptions 
  ( BeeGfsCommand(..)
  , GetQuotaOpts(..)
  , SetQuotaOpts(..)
  , opts
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

-- Data types to represent the different commands
data BeeGfsCommand 
    = GetQuota GetQuotaOpts
    | SetQuota SetQuotaOpts
    deriving (Show)

-- Options for get-quota command
data GetQuotaOpts = GetQuotaOpts
    { gqCsv :: Bool
    , gqGid :: Bool
    , gqUid :: Bool
    , gqMount :: FilePath
    , gqUserList :: Maybe String
    , gqUidValue :: Maybe String
    , gqGidValue :: Maybe String
    } deriving (Show)

-- Options for set-quota command
data SetQuotaOpts = SetQuotaOpts
    { sqGid :: Maybe String
    , sqUid :: Maybe String
    , sqSizeLimit :: String
    , sqInodeLimit :: Maybe String
    , sqMount :: FilePath
    , sqUnlimitedInodes :: Bool
    } deriving (Show)

-- Parser for get-quota command
getQuotaOpts :: Parser GetQuotaOpts
getQuotaOpts = GetQuotaOpts
    <$> switch
        ( long "csv"
        <> help "Output in CSV format" )
    <*> switch
        ( long "gid"
        <> help "Use group ID" )
    <*> switch
        ( long "uid"
        <> help "Use user ID" )
    <*> strOption
        ( long "mount"
        <> metavar "MOUNT_POINT"
        <> help "Mount point to check quota for (e.g., /project, /home1)"
        <> value "/project" )
    <*> optional (strOption
        ( long "list"
        <> metavar "USER_LIST"
        <> help "List of users/groups to check" ))
    <*> optional (strOption
        ( long "uid"
        <> metavar "UID"
        <> help "User ID to check quota for" ))
    <*> optional (strOption
        ( long "gid"
        <> metavar "GID"
        <> help "Group ID to check quota for" ))

-- Parser for set-quota command
setQuotaOpts :: Parser SetQuotaOpts
setQuotaOpts = SetQuotaOpts
    <$> optional (strOption
        ( long "gid"
        <> metavar "GID"
        <> help "Group ID to set quota for" ))
    <*> optional (strOption
        ( long "uid"
        <> metavar "UID"
        <> help "User ID to set quota for" ))
    <*> strOption
        ( long "sizelimit"
        <> metavar "SIZE"
        <> help "Storage size limit (e.g., 1T, 500G)" )
    <*> optional (strOption
        ( long "inodelimit"
        <> metavar "INODES"
        <> help "Inode limit (default: 30M)" ))
    <*> strOption
        ( long "mount"
        <> metavar "MOUNT_POINT"
        <> help "Mount point to set quota for (e.g., /project, /home1)"
        <> value "/project" )
    <*> switch
        ( long "unlimited-inodes"
        <> help "Set unlimited inodes" )

-- Main command parser
beegfsCommand :: Parser BeeGfsCommand
beegfsCommand = subparser
    ( command "getquota" 
        (info (GetQuota <$> getQuotaOpts)
            ( progDesc "Get quota information for users/groups" ))
    <> command "setquota"
        (info (SetQuota <$> setQuotaOpts)
            ( progDesc "Set quota for a group" ))
    )

-- Main options parser with common flags
opts :: ParserInfo BeeGfsCommand
opts = info (beegfsCommand <**> helper)
    ( fullDesc
    <> progDesc "BeeGFS quota management tool"
    <> header "beegfs-quota - manage BeeGFS quotas" )
