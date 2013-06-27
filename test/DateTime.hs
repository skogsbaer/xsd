{-# LANGUAGE OverloadedStrings #-}

module DateTime (tests) where

import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit hiding (Test)
import           Data.Fixed (Pico)
import qualified Data.Text as T
import           Data.Time
import           Test.QuickCheck.Instances ()

import           Text.XML.XSD

mkUTC :: Integer -> Int -> Int -> Int -> Int -> Pico -> UTCTime
mkUTC y m d hh mm ss = localTimeToUTC utc (mkLocal y m d hh mm ss)

mkZoned :: Integer -> Int -> Int -> Int -> Int -> Pico -> Int -> Int -> ZonedTime
mkZoned y m d hh mm ss zh zm = ZonedTime (mkLocal y m d hh mm ss)
                                         (TimeZone offset False "")
  where
    offset = signum zh * (abs zh * 60 + zm)

mkLocal :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
mkLocal y m d hh mm ss = LocalTime (fromGregorian y m d)
                                   (TimeOfDay hh mm ss)

prop_UtcAndBack :: UTCTime -> Bool
prop_UtcAndBack t = Just (DtZoned t) == dateTime (toText . fromUTCTime $ t)

prop_LocalTimeAndBack :: LocalTime -> Bool
prop_LocalTimeAndBack t = Just (DtUnzoned t) == dateTime (toText . fromLocalTime $ t)

cases_GoodExamples :: [Test]
cases_GoodExamples = map mkCase $ zip input expected
  where
    mkCase (t, dt) = testCase (T.unpack t) $ dateTime' t @?= Right dt
    input = [ "2009-10-10T03:10:10-05:00"
            , "2119-10-10T03:10:10.4-13:26"
            , "0009-10-10T03:10:10.783952+14:00"
            , "2009-10-10T03:10:10Z"
            , "-2009-05-10T21:08:59+05:00"
            , "-19399-12-31T13:10:10-14:00"
            , "2009-12-31T13:10:10"
            , "2012-10-15T24:00:00"
            , "2002-10-10T12:00:00+05:00"
            , "2002-10-10T00:00:00+05:00"
            , "-0001-10-10T00:00:00"
            , "0001-10-10T00:00:00"
            ]
    expected = [ DtZoned $ mkUTC 2009 10 10 8 10 10
               , DtZoned $ mkUTC 2119 10 10 16 36 10.4
               , DtZoned $ mkUTC 0009 10 9 13 10 10.783952
               , DtZoned $ mkUTC 2009 10 10 03 10 10
               , DtZoned $ mkUTC (-2009) 5 10 16 8 59
               , DtZoned $ mkUTC (-19398) 1 1 3 10 10
               , DtUnzoned $ mkLocal 2009 12 31 13 10 10
               , DtUnzoned $ mkLocal 2012 10 16 0 0 0
               , DtZoned $ mkUTC 2002 10 10 7 0 0
               , DtZoned $ mkUTC 2002 10 9 19 0 0
               , DtUnzoned $ mkLocal (-1) 10 10 0 0 0
               , DtUnzoned $ mkLocal 1 10 10 0 0 0
               ]

cases_BadExamples :: [Test]
cases_BadExamples = map mkCase input
  where
    mkCase t = testCase (T.unpack t) $ dateTime t @?= Nothing
    input = [ "2009-10-10T03:10:10-05"    -- Zone offset without minutes.
            , "2009-10-10T03:10:10+14:50" -- Zone offset beyond 14 hours.
            , "2009-10-10T03:10:1"        -- One digit second.
            , "2009-10-10T03:1:10"        -- One digit minute.
            , "2009-10-10T0:10:10"        -- One digit hour.
            , "2009-10-1T10:10:10"        -- One digit day.
            , "2009-1-10T10:10:10"        -- One digit month.
            , "209-10-10T03:10:10"        -- Three digit year.
            , "2009-10-10T24:10:10"       -- Time past midnight.
            , "0000-01-01T00:00:00"       -- Year zero.
            , "2009-13-01T00:00:00"       -- Month 13.
            , "+2009-10-01T04:20:40"      -- '+' before year.
            , "002009-10-01T04:20:40"     -- '0' in prefix position of year >999.
            ]

cases_ToText :: [Test]
cases_ToText = map mkCase $ zip input expected
  where
    mkCase (t, e) = testCase (T.unpack e) $ toText t @?= e
    input = [ fromUTCTime $ mkUTC 2119 10 10 16 36 10.4
            , fromZonedTime $ mkZoned 2010 04 07 13 47 20.001 2 0
            , fromLocalTime $ mkLocal 13 2 4 20 20 20
            ]
    expected = [ "2119-10-10T16:36:10.4Z"
               , "2010-04-07T11:47:20.001Z"
               , "0013-02-04T20:20:20"
               ]

case_Issue2 :: Assertion
case_Issue2 = result @?= expected
  where
    input = "2010-04-07T13:47:20.001+02:00"
    expected = Just "2010-04-07T11:47:20.001Z"
    result = fmap toText (dateTime input)

tests :: Test
tests = testGroup "DateTime"
        [ testProperty "UTC round-trip" prop_UtcAndBack
        , testProperty "LocalTime round-trip" prop_LocalTimeAndBack
        , testGroup "Good parse examples" cases_GoodExamples
        , testGroup "Bad parse examples" cases_BadExamples
        , testGroup "To text" cases_ToText
        , testCase "Example from issue 2" case_Issue2
        ]
