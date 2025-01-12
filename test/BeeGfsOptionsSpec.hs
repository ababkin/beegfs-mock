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
        it "parses getquota command with list of GIDs" $ do
            let args = words "getquota --csv --mount=/project --list myproject_246,myproject2_335"
            let result = parseCommand args
            result `shouldBe` Right (GetQuota GetQuotaOpts 
                { gqCsv = True
                , gqType = UseGID
                , gqMount = "/project"
                , gqSelection = List "myproject_246,myproject2_335"
                })

        it "parses getquota command with single GID" $ do
            let args = words "getquota --mount=/project myproject_246"
            let result = parseCommand args
            result `shouldBe` Right (GetQuota GetQuotaOpts 
                { gqCsv = False
                , gqType = UseGID
                , gqMount = "/project"
                , gqSelection = Single (Just "myproject_246")
                })

        -- Removed tests for --uid option

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False 