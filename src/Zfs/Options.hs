{-# LANGUAGE OverloadedStrings #-}

module Zfs.Options
    ( ZfsCommand(..)
    , parseZfsOpts
    , opts
    ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Text.Regex.TDFA ((=~))
import Control.Applicative (Alternative(..))
import Data.List (isPrefixOf)

-- | Main ZFS command types
data ZfsCommand
    = GetQuota String      -- ^ Get quota for username
    | SetStorageQuota String String  -- ^ Set storage quota: username and size (e.g., "100G")
    | SetObjectQuota String String   -- ^ Set object quota: username and count (e.g., "2000000")
    deriving (Show, Eq)

-- | Extract username from quota string
extractUsername :: String -> String -> Maybe String
extractUsername input pattern = 
    case input =~ pattern :: (String, String, String, [String]) of
        (_, _, _, [username]) -> Just username
        _ -> Nothing

-- | Extract username and value from quota string
extractUsernameAndValue :: String -> String -> Maybe (String, String)
extractUsernameAndValue input pattern =
    case input =~ pattern :: (String, String, String, [String]) of
        (_, _, _, [username, value]) -> Just (username, value)
        _ -> Nothing

-- | Parser for getting quota information
getQuotaParser :: Mod CommandFields ZfsCommand
getQuotaParser = command "get" $ info
    (GetQuota 
        <$> (flag' () (short 'H') 
             *> (extractQuotaUsername 
                 <$> strArgument (metavar "QUOTA" <> help "Quota specification (e.g., userquota@username)")))
        <* optional (strArgument (metavar "DATASET" <> help "ZFS dataset name")))
    (progDesc "Get quota information")
  where
    extractQuotaUsername quota = 
        case extractUsername quota "userquota@([^[:space:]]+)" of
            Just username -> username
            Nothing -> quota  -- fallback to original input if pattern doesn't match

-- | Parser for setting quotas
setQuotaParser :: Mod CommandFields ZfsCommand
setQuotaParser = command "set" $ info
    (argument (eitherReader parseQuota)
        (metavar "QUOTA=VALUE" <> help "Format: userquota@USERNAME=SIZE or userobjquota@USERNAME=COUNT")
        <* optional (strArgument (metavar "DATASET" <> help "ZFS dataset name")))
    (progDesc "Set quota values")
  where
    parseQuota input =
        case extractUsernameAndValue input "userquota@([^=]+)=([^[:space:]]+)" of
            Just (username, size) | "userquota@" `isPrefixOf` input -> 
                Right $ SetStorageQuota username size
            _ -> case extractUsernameAndValue input "userobjquota@([^=]+)=([^[:space:]]+)" of
                Just (username, count) | "userobjquota@" `isPrefixOf` input -> 
                    Right $ SetObjectQuota username count
                _ -> Left "Invalid quota format. Expected: userquota@USERNAME=SIZE or userobjquota@USERNAME=COUNT"

-- | Command parser combining all subcommands
commandParser :: Parser ZfsCommand
commandParser = hsubparser
    ( getQuotaParser
    <> setQuotaParser
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

