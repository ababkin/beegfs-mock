{-# LANGUAGE OverloadedStrings #-}

module BeeGfsOptionsSpec (spec) where

import Test.Hspec
import Options.Applicative
import Data.Either (isRight)

import BeeGfsOptions

-- Helper function to parse commands
parseCommand :: [String] -> Either String BeeGfsCommand
parseCommand args = case execParserPure defaultPrefs opts args of
    Success cmd -> Right cmd
    Failure f -> Left $ show f
    CompletionInvoked _ -> Left "completion invoked"

spec :: Spec
spec = do
    describe "BeeGFS command parsing" $ do
        it "parses getquota command with UID" $ do
            let args = words "--getquota --csv --uid USERNAME --mount=/home1"
            let result = parseCommand args
            result `shouldBe` Right (GetQuota GetQuotaOpts 
                { gqCsv = True
                , gqType = UseUID
                , gqMount = "/home1"
                , gqSelection = Single (Just "USERNAME")
                })

        it "parses getquota command with single GID" $ do
            let args = words "--getquota --csv --gid --mount=/project --list group1"
            let result = parseCommand args
            result `shouldBe` Right (GetQuota GetQuotaOpts 
                { gqCsv = True
                , gqType = UseGID
                , gqMount = "/project"
                , gqSelection = List "group1"
                })

        it "parses getquota command with multiple GIDs" $ do
            let args = words "--getquota --csv --gid --mount=/project --list group1,group2,group3"
            let result = parseCommand args
            result `shouldBe` Right (GetQuota GetQuotaOpts 
                { gqCsv = True
                , gqType = UseGID
                , gqMount = "/project"
                , gqSelection = List "group1,group2,group3"
                })

        it "parses setquota command with unlimited inodes" $ do
            let args = words "--setquota --gid research_group_123 --sizelimit=10T --inodelimit=unlimited --mount=/project"
            let result = parseCommand args
            result `shouldBe` Right (SetQuota SetQuotaOpts 
                { sqGid = "research_group_123"
                , sqSizeLimit = "10T"
                , sqInodeLimit = "unlimited"
                , sqMount = "/project"
                , sqUnlimitedInodes = False
                })

        it "parses setquota command with specific inode limit" $ do
            let args = words "--setquota --gid research_group_123 --sizelimit=5T --inodelimit=30000000 --mount=/project"
            let result = parseCommand args
            result `shouldBe` Right (SetQuota SetQuotaOpts 
                { sqGid = "research_group_123"
                , sqSizeLimit = "5T"
                , sqInodeLimit = "30000000"
                , sqMount = "/project"
                , sqUnlimitedInodes = False
                })

        it "parses setquota command with positional arguments" $ do
            let args = words "--setquota --gid research_group_123 --sizelimit 1T /home1/username"
            let result = parseCommand args
            result `shouldBe` Right (SetQuota SetQuotaOpts 
                { sqGid = "research_group_123"
                , sqSizeLimit = "1T"
                , sqInodeLimit = "unlimited"
                , sqMount = "/home1/username"
                , sqUnlimitedInodes = False
                })

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False 