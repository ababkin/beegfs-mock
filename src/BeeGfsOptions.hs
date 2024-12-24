{-# LANGUAGE RecordWildCards #-}

module BeeGfsOptions 
  ( BeeGfsCommand(..)
  , GetQuotaOpts(..)
  , SetQuotaOpts(..)
  , QuotaType(..)
  , QuotaSelection(..)
  , opts
  ) where

import Options.Applicative
    ( Parser
    , ParserInfo
    , info
    , switch
    , long
    , help
    , strOption
    , metavar
    , value
    , argument
    , str
    , optional
    , subparser
    , command
    , progDesc
    , (<**>)
    , helper
    , fullDesc
    , header
    )

-- Data types to represent the different commands
data BeeGfsCommand 
    = GetQuota GetQuotaOpts
    | SetQuota SetQuotaOpts
    deriving (Show)

-- Options for get-quota command
data QuotaType = UseUID | UseGID
    deriving (Show, Eq)

data QuotaSelection 
    = Single (Maybe String)  -- Single UID/GID value
    | List String           -- List of UIDs/GIDs
    | All                   -- All UIDs/GIDs
    | Range String String   -- Range of UIDs/GIDs
    deriving (Show)

data GetQuotaOpts = GetQuotaOpts
    { gqCsv :: Bool
    , gqType :: QuotaType
    , gqMount :: FilePath
    , gqSelection :: QuotaSelection
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
    <*> (makeQuotaType
        <$> switch ( long "uid" <> help "Use user ID" )
        <*> switch ( long "gid" <> help "Use group ID" ))
    <*> strOption
        ( long "mount"
        <> metavar "MOUNT_POINT"
        <> help "Mount point to check quota for (e.g., /project, /home1)"
        <> value "/project" )
    <*> (makeSelection
        <$> switch ( long "all" <> help "Query all UIDs/GIDs" )
        <*> optional (strOption 
            ( long "list"
            <> metavar "ID_LIST"
            <> help "Comma-separated list of IDs to check" ))
        <*> optional (strOption
            ( long "range-start"
            <> metavar "START"
            <> help "Start of ID range" ))
        <*> optional (strOption
            ( long "range-end"
            <> metavar "END"
            <> help "End of ID range" ))
        <*> optional (argument str
            ( metavar "ID"
            <> help "Single ID to check" )))
  where
    makeQuotaType uid gid = case (uid, gid) of
        (True, False) -> UseUID
        (False, True) -> UseGID
        _ -> error "One of --uid or --gid is required"

    makeSelection all lst start end single = case (all, lst, start, end, single) of
        (True, Nothing, Nothing, Nothing, Nothing) -> All
        (False, Just ids, Nothing, Nothing, Nothing) -> List ids
        (False, Nothing, Just s, Just e, Nothing) -> Range s e
        (False, Nothing, Nothing, Nothing, s) -> Single s
        _ -> error "Only one selection method allowed"

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
