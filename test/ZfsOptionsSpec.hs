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
            let args = words "get --username john_doe"
            let result = parseCommand args
            result `shouldBe` Right (GetQuota "john_doe")

        it "parses set storage quota command with default size" $ do
            let args = words "set-storage --username john_doe"
            let result = parseCommand args
            result `shouldBe` Right (SetStorageQuota "john_doe" "100G")

        it "parses set storage quota command with custom size" $ do
            let args = words "set-storage --username john_doe --size 500G"
            let result = parseCommand args
            result `shouldBe` Right (SetStorageQuota "john_doe" "500G")

        it "parses set object quota command with default count" $ do
            let args = words "set-object --username john_doe"
            let result = parseCommand args
            result `shouldBe` Right (SetObjectQuota "john_doe" "2000000")

        it "parses set object quota command with custom count" $ do
            let args = words "set-object --username john_doe --count 1000000"
            let result = parseCommand args
            result `shouldBe` Right (SetObjectQuota "john_doe" "1000000")

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False 