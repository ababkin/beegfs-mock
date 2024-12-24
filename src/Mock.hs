module Mock where

import Options.Applicative
import Text.Printf (printf)
import Data.List (intercalate)
import BeeGfsOptions

-- Data types for quota information
data QuotaInfo = QuotaInfo
  { username :: String
  , usedSpace :: Integer    -- in bytes
  , spaceLimit :: Integer   -- in bytes
  , usedInodes :: Integer
  , inodeLimit :: Integer
  } deriving (Show)

-- Mock data
defaultQuota :: QuotaInfo
defaultQuota = QuotaInfo
  { username = "default"
  , usedSpace = 1024 * 1024 * 100  -- 100MB
  , spaceLimit = 1024 * 1024 * 1000  -- 1GB
  , usedInodes = 1000
  , inodeLimit = 10000
  }

-- Format size in human-readable format
formatSize :: Integer -> String
formatSize bytes
  | bytes >= 1024 * 1024 * 1024 = printf "%.2f GB" (fromIntegral bytes / (1024 * 1024 * 1024) :: Double)
  | bytes >= 1024 * 1024 = printf "%.2f MB" (fromIntegral bytes / (1024 * 1024) :: Double)
  | bytes >= 1024 = printf "%.2f KB" (fromIntegral bytes / 1024 :: Double)
  | otherwise = show bytes ++ " B"

-- Display quota information
displayQuota :: QuotaInfo -> String
displayQuota qi = intercalate "\n"
  [ "User: " ++ username qi
  , "Space used: " ++ formatSize (usedSpace qi)
  , "Space limit: " ++ formatSize (spaceLimit qi)
  , "Inodes used: " ++ show (usedInodes qi)
  , "Inode limit: " ++ show (inodeLimit qi)
  ]


-- Main function to run the mock
runMock :: IO ()
runMock = execParser opts >>= handleCommand


handleCommand :: BeeGfsCommand -> IO ()
handleCommand (GetQuota gopts) = handleGetQuota gopts
handleCommand (SetQuota sopts) = handleSetQuota sopts

-- Handler functions
handleGetQuota :: GetQuotaOpts -> IO ()
handleGetQuota opts = do
  let args = ["beegfs-ctl", "--getquota"]
        ++ ["--csv" | gqCsv opts]
        ++ ["--gid" | gqGid opts]
        ++ ["--uid" | gqUid opts]
        ++ ["--mount=" ++ gqMount opts]
        ++ maybe [] (\l -> ["--list", l]) (gqUserList opts)
        ++ maybe [] (\uid -> ["--uid", uid]) (gqUidValue opts)
        ++ maybe [] (\gid -> ["--gid", gid]) (gqGidValue opts)
  putStrLn $ "Would execute: " ++ unwords args

handleSetQuota :: SetQuotaOpts -> IO ()
handleSetQuota opts = do
  let defaultInodes = "30000000"  -- 30M default inode limit
      inodeArgs = if sqUnlimitedInodes opts 
                 then ["--inodelimit=unlimited"]
                 else maybe ["--inodelimit=" ++ defaultInodes] 
                      (\i -> ["--inodelimit=" ++ i]) 
                      (sqInodeLimit opts)
      args = ["beegfs-ctl", "--setquota"]
        ++ maybe [] (\gid -> ["--gid", gid]) (sqGid opts)
        ++ maybe [] (\uid -> ["--uid", uid]) (sqUid opts)
        ++ ["--sizelimit=" ++ sqSizeLimit opts]
        ++ inodeArgs
        ++ ["--mount=" ++ sqMount opts]
  putStrLn $ "Would execute: " ++ unwords args