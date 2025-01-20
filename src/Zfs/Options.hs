{-# LANGUAGE OverloadedStrings #-}

module Zfs.Options
    ( ZfsCommand(..)
    , parseZfsOpts
    , opts
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

-- | Main ZFS command types
data ZfsCommand
    = GetQuota String      -- ^ Get quota for username
    | SetStorageQuota String String  -- ^ Set storage quota: username and size (e.g., "100G")
    | SetObjectQuota String String   -- ^ Set object quota: username and count (e.g., "2000000")
    deriving (Show, Eq)

-- | Parser for getting quota information
getQuotaParser :: Parser ZfsCommand
getQuotaParser = GetQuota
    <$> strOption
        ( long "username"
        <> short 'u'
        <> metavar "USERNAME"
        <> help "Username to query quota for"
        )

-- | Parser for setting storage quota
setStorageQuotaParser :: Parser ZfsCommand
setStorageQuotaParser = SetStorageQuota
    <$> strOption
        ( long "username"
        <> short 'u'
        <> metavar "USERNAME"
        <> help "Username to set quota for"
        )
    <*> strOption
        ( long "size"
        <> short 's'
        <> metavar "SIZE"
        <> help "Storage quota size (e.g., '100G')"
        <> value "100G"
        )

-- | Parser for setting object quota
setObjectQuotaParser :: Parser ZfsCommand
setObjectQuotaParser = SetObjectQuota
    <$> strOption
        ( long "username"
        <> short 'u'
        <> metavar "USERNAME"
        <> help "Username to set quota for"
        )
    <*> strOption
        ( long "count"
        <> short 'c'
        <> metavar "COUNT"
        <> help "Object count limit"
        <> value "2000000"
        )

-- | Command parser combining all subcommands
commandParser :: Parser ZfsCommand
commandParser = subparser
    ( command "get"
        (info getQuotaParser
        (progDesc "Get ZFS quota information for a user"))
    <> command "set-storage"
        (info setStorageQuotaParser
        (progDesc "Set ZFS storage quota for a user"))
    <> command "set-object"
        (info setObjectQuotaParser
        (progDesc "Set ZFS object quota for a user"))
    )

-- | Main options parser with description
opts :: ParserInfo ZfsCommand
opts = info (commandParser <**> helper)
    ( fullDesc
    <> progDesc "ZFS quota management tool"
    <> header "zfs-quota - manage ZFS quotas for users"
    )

parseZfsOpts :: IO ZfsCommand
parseZfsOpts = execParser opts
