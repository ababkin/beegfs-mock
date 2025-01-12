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
    , option
    , auto
    , flag'
    , (<|>)
    )

-- Data types to represent the different commands
data BeeGfsCommand 
    = GetQuota GetQuotaOpts
    | SetQuota SetQuotaOpts
    deriving (Show, Eq)

-- Options for get-quota command
data QuotaType = UseGID
    deriving (Show, Eq)

data QuotaSelection 
    = Single (Maybe String)  -- Single UID/GID value
    | List String           -- List of UIDs/GIDs
    | All                   -- All UIDs/GIDs
    | Range String String   -- Range of UIDs/GIDs
    deriving (Show, Eq)

data GetQuotaOpts = GetQuotaOpts
    { gqCsv :: Bool
    , gqType :: QuotaType
    , gqMount :: FilePath
    , gqSelection :: QuotaSelection
    } deriving (Show, Eq)

-- Options for set-quota command
data SetQuotaOpts = SetQuotaOpts
    { sqGid :: String
    , sqSizeLimit :: String
    , sqInodeLimit :: String
    , sqMount :: FilePath
    , sqUnlimitedInodes :: Bool
    } deriving (Show, Eq)

-- Parser for get-quota command
getQuotaOpts :: Parser GetQuotaOpts
getQuotaOpts = GetQuotaOpts
    <$> switch
        ( long "csv"
        <> help "Output in CSV format" )
    <*> pure UseGID
    <*> strOption
        ( long "mount"
        <> metavar "MOUNT_POINT"
        <> help "Mount point to check quota for (e.g., /project, /home1)"
        <> value "/project" )
    <*> (makeSelection
        <$> switch ( long "all" <> help "Query all GIDs" )
        <*> optional (strOption 
            ( long "list"
            <> metavar "GID_LIST"
            <> help "Comma-separated list of GIDs to check" ))
        <*> optional (strOption
            ( long "range-start"
            <> metavar "START"
            <> help "Start of GID range" ))
        <*> optional (strOption
            ( long "range-end"
            <> metavar "END"
            <> help "End of GID range" ))
        <*> optional (argument str
            ( metavar "GID"
            <> help "Single GID to check" )))
  where
    makeSelection all lst start end single = case (all, lst, start, end, single) of
        (True, Nothing, Nothing, Nothing, Nothing) -> All
        (False, Just ids, Nothing, Nothing, Nothing) -> List ids
        (False, Nothing, Just s, Just e, Nothing) -> Range s e
        (False, Nothing, Nothing, Nothing, s) -> Single s
        _ -> error "Only one selection method allowed"

-- Parser for set-quota command
setQuotaOpts :: Parser SetQuotaOpts
setQuotaOpts = SetQuotaOpts
    <$> strOption
        ( long "gid"
        <> metavar "LDAPGIDNUMBER"
        <> help "Group ID to set quota for" )
    <*> strOption
        ( long "sizelimit"
        <> metavar "PROJECTSIZET"
        <> help "Storage size limit (e.g., 1T, 500G)" )
    <*> strOption
        ( long "inodelimit"
        <> metavar "INODES"
        <> help "Inode limit (e.g., unlimited, 30M)" )
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
beegfsCommand = 
    (SetQuota <$> setQuotaParser) <|> 
    (GetQuota <$> getQuotaParser)
  where
    setQuotaParser = flag' SetQuotaOpts
        ( long "setquota"
        <> help "Set quota for a group" )
        <*> strOption
            ( long "gid"
            <> metavar "LDAPGIDNUMBER"
            <> help "Group ID to set quota for" )
        <*> strOption
            ( long "sizelimit"
            <> metavar "PROJECTSIZET"
            <> help "Storage size limit (e.g., 1T, 500G)" )
        <*> strOption
            ( long "inodelimit"
            <> metavar "INODES"
            <> help "Inode limit (e.g., unlimited, 30M)" )
        <*> strOption
            ( long "mount"
            <> metavar "MOUNT_POINT"
            <> help "Mount point to set quota for (e.g., /project, /home1)"
            <> value "/project" )
        <*> switch
            ( long "unlimited-inodes"
            <> help "Set unlimited inodes" )

    getQuotaParser = flag' GetQuotaOpts
        ( long "getquota"
        <> help "Get quota information for groups" )
        <*> switch
            ( long "csv"
            <> help "Output in CSV format" )
        <*> pure UseGID
        <*> strOption
            ( long "mount"
            <> metavar "MOUNT_POINT"
            <> help "Mount point to check quota for (e.g., /project, /home1)"
            <> value "/project" )
        <*> (makeSelection
            <$> switch ( long "all" <> help "Query all GIDs" )
            <*> optional (strOption 
                ( long "list"
                <> metavar "GID_LIST"
                <> help "Comma-separated list of GIDs to check" ))
            <*> optional (strOption
                ( long "range-start"
                <> metavar "START"
                <> help "Start of GID range" ))
            <*> optional (strOption
                ( long "range-end"
                <> metavar "END"
                <> help "End of GID range" ))
            <*> optional (argument str
                ( metavar "GID"
                <> help "Single GID to check" )))

    makeSelection all lst start end single = case (all, lst, start, end, single) of
        (True, Nothing, Nothing, Nothing, Nothing) -> All
        (False, Just ids, Nothing, Nothing, Nothing) -> List ids
        (False, Nothing, Just s, Just e, Nothing) -> Range s e
        (False, Nothing, Nothing, Nothing, s) -> Single s
        _ -> error "Only one selection method allowed"

-- Main options parser with common flags
opts :: ParserInfo BeeGfsCommand
opts = info (beegfsCommand <**> helper)
    ( fullDesc
    <> progDesc "BeeGFS quota management tool"
    <> header "beegfs-quota - manage BeeGFS quotas" )
