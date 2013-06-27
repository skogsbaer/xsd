{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Time.Clock (UTCTime)
import           Debug.Trace
import           Test.Framework (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit hiding (Test)
--import Test.QuickCheck ()
import qualified Data.Text as T
import           Test.QuickCheck.Instances ()

import           Text.XML.XSD

import           DateTime

booleanTests :: Test
booleanTests = testGroup "Boolean"
               $ parseTests ++ renderTests
  where
    mkParseTest (t, e) = testCase ("Parse " ++ T.unpack t)
                                  (boolean t @?= Right e)
    parseTests = map mkParseTest [ ("1", True)
                                 , ("true", True)
                                 , ("0", False)
                                 , ("false", False)
                                 ]
    renderTests =
      [ testCase "Render True" $ toBoolean True @?= "true"
      , testCase "Render False" $ toBoolean False @?= "false"
      ]

main :: IO ()
main = defaultMain [ booleanTests
                   , DateTime.tests
                   ]
