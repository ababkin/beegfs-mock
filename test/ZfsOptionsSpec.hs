{-# LANGUAGE OverloadedStrings #-}

module ZfsOptionsSpec (spec) where

import Test.Hspec
import Options.Applicative
import Data.Either (isRight)

import Zfs.Options
    ( ZfsCommand(..)
    , opts
    )

-- Helper function to parse commands
parseCommand :: [String] -> Either String ZfsCommand
parseCommand args = case execParserPure defaultPrefs opts args of
    Success cmd -> Right cmd
    Failure f -> Left $ show f
    CompletionInvoked _ -> Left "completion invoked"

spec :: Spec
spec = do
    describe "ZFS command parsing" $ do
        it "parses get quota command" $ do
            let args = words "get -H userquota@john_doe home1"
            let result = parseCommand args
            result `shouldBe` Right (GetQuota "john_doe")

        it "parses set storage quota command with default size" $ do
            let args = words "set userquota@john_doe=100G home1"
            let result = parseCommand args
            result `shouldBe` Right (SetStorageQuota "john_doe" "100G")

        it "parses set storage quota command with custom size" $ do
            let args = words "set userquota@john_doe=500G home1"
            let result = parseCommand args
            result `shouldBe` Right (SetStorageQuota "john_doe" "500G")

        it "parses set object quota command with default count" $ do
            let args = words "set userobjquota@john_doe=2000000 home1"
            let result = parseCommand args
            result `shouldBe` Right (SetObjectQuota "john_doe" "2000000")

        it "parses set object quota command with custom count" $ do
            let args = words "set userobjquota@john_doe=1000000 home1"
            let result = parseCommand args
            result `shouldBe` Right (SetObjectQuota "john_doe" "1000000")

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False 