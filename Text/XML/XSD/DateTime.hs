{-# LANGUAGE OverloadedStrings #-}

-- | XSD @dateTime@ data structure <http://www.w3.org/TR/xmlschema-2/#dateTime>
module Text.XML.XSD.DateTime
       ( DateTime(..)
       , isZoned
       , isUnzoned
       , dateTime'
       , dateTime
       , toText
       , fromZonedTime
       , toUTCTime
       , fromUTCTime
       , toLocalTime
       , fromLocalTime
       , utcTime'
       , utcTime
       , localTime'
       , localTime
       ) where

import           Control.Applicative (pure, (<$>), (*>), (<|>))
import           Control.Monad (when)
import           Data.Attoparsec.Text (Parser, char, digit)
import qualified Data.Attoparsec.Text as A
import           Data.Char (isDigit, ord)
import           Data.Fixed (Pico, showFixed)
import           Data.Maybe (maybeToList)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Read as TR
import           Data.Time
import           Data.Time.Calendar.MonthDay (monthLength)

-- | XSD @dateTime@ data structure
-- <http://www.w3.org/TR/xmlschema-2/#dateTime>. Briefly, a @dateTime@
-- uses the Gregorian calendar and may or may not have an associated
-- timezone. If it has a timezone, then the canonical representation
-- of that date time is in UTC.
--
-- Note, it is not possible to establish a total order on @dateTime@
-- since non-timezoned are considered to belong to some unspecified
-- timezone.
data DateTime = DtZoned UTCTime
              | DtUnzoned LocalTime
              deriving (Eq)

-- | Internal helper that creates a date time. Note, if the given hour
-- is 24 then the minutes and seconds are assumed to be 0.
mkDateTime :: Integer           -- ^ Year
           -> Int               -- ^ Month
           -> Int               -- ^ Day
           -> Int               -- ^ Hours
           -> Int               -- ^ Minutes
           -> Pico              -- ^ Seconds
           -> Maybe Pico        -- ^ Time zone offset
           -> DateTime
mkDateTime y m d hh mm ss mz =
  case mz of
    Just z -> DtZoned $ addUTCTime (negate $ realToFrac z) uTime
    Nothing -> DtUnzoned lTime
  where
    day = addDays (if hh == 24 then 1 else 0) (fromGregorian y m d)
    tod = TimeOfDay (if hh == 24 then 0 else hh) mm ss
    lTime = LocalTime day tod
    uTime = UTCTime day (timeOfDayToTime tod)

instance Show DateTime where
  show = T.unpack . toText

instance Read DateTime where
  readList s = [(maybeToList (dateTime . T.pack $ s), [])]

-- | Parses the string into a @dateTime@ or may fail with a parse error.
dateTime' :: Text -> Either String DateTime
dateTime' = A.parseOnly (parseDateTime <|> fail "bad date time")

-- | Parses the string into a @dateTime@ or may fail.
dateTime :: Text -> Maybe DateTime
dateTime = either (const Nothing) Just . dateTime'

toText :: DateTime -> Text
toText = TL.toStrict . TB.toLazyText . dtBuilder
  where
    dtBuilder (DtZoned uTime) = ltBuilder (utcToLocalTime utc uTime) <> "Z"
    dtBuilder (DtUnzoned lTime) = ltBuilder lTime
    ltBuilder (LocalTime day (TimeOfDay hh mm sss)) =
      let (y, m, d) = toGregorian day
      in  buildInt4 y
          <> "-"
          <> buildUInt2 m
          <> "-"
          <> buildUInt2 d
          <> "T"
          <> buildUInt2 hh
          <> ":"
          <> buildUInt2 mm
          <> ":"
          <> buildSeconds sss

buildInt4 :: Integer -> TB.Builder
buildInt4 year =
  let absYear = abs year
      k x = if absYear < x then ("0" <>) else id
  in  k 1000 . k 100 . k 10 $ TBI.decimal year

buildUInt2 :: Int -> TB.Builder
buildUInt2 x = (if x < 10 then ("0" <>) else id) $ TBI.decimal x

buildSeconds :: Pico -> TB.Builder
buildSeconds secs = (if secs < 10 then ("0" <>) else id)
                    $ TB.fromString (showFixed True secs)

-- | Converts a zoned time to a @dateTime@.
fromZonedTime :: ZonedTime -> DateTime
fromZonedTime = fromUTCTime . zonedTimeToUTC

-- | Whether the given @dateTime@ is timezoned.
isZoned :: DateTime -> Bool
isZoned (DtZoned _) = True
isZoned (DtUnzoned _) = False

-- | Whether the given @dateTime@ is non-timezoned.
isUnzoned :: DateTime -> Bool
isUnzoned = not . isZoned

-- | Attempts to convert a @dateTime@ to a UTC time. The attempt fails
-- if the given @dateTime@ is non-timezoned.
toUTCTime :: DateTime -> Maybe UTCTime
toUTCTime (DtZoned time) = Just time
toUTCTime _ = Nothing

-- | Converts a UTC time to a timezoned @dateTime@.
fromUTCTime :: UTCTime -> DateTime
fromUTCTime = DtZoned

-- | Attempts to convert a @dateTime@ to a local time. The attempt
-- fails if the given @dateTime@ is timezoned.
toLocalTime :: DateTime -> Maybe LocalTime
toLocalTime (DtUnzoned time) = Just time
toLocalTime _ = Nothing

-- | Converts a local time to an non-timezoned @dateTime@.
fromLocalTime :: LocalTime -> DateTime
fromLocalTime = DtUnzoned

-- | Parses the string in a @dateTime@ then converts to a UTC time and
-- may fail with a parse error.
utcTime' :: Text -> Either String UTCTime
utcTime' txt = dateTime' txt >>= maybe (Left err) Right . toUTCTime
  where
    err = "input time is non-timezoned"

-- | Parses the string in a @dateTime@ then converts to a UTC time and
-- may fail.
utcTime :: Text -> Maybe UTCTime
utcTime txt = dateTime txt >>= toUTCTime

-- | Parses the string in a @dateTime@ then converts to a local time
-- and may fail with a parse error.
localTime' :: Text -> Either String LocalTime
localTime' txt = dateTime' txt >>= maybe (Left err) Right . toLocalTime
  where
    err = "input time is non-timezoned"

-- | Parses the string in a @dateTime@ then converts to a local time
-- time and may fail.
localTime :: Text -> Maybe LocalTime
localTime txt = dateTime txt >>= toLocalTime

-- | Parser of the @dateTime@ lexical representation.
parseDateTime :: Parser DateTime
parseDateTime = do yy <- yearParser
                   _ <- char '-'
                   mm <- p2imax 12
                   _ <- char '-'
                   dd <- p2imax (monthLength (isLeapYear $ fromIntegral yy) mm)
                   _ <- char 'T'
                   hhh <- p2imax 24
                   _ <- char ':'
                   mmm <- p2imax 59
                   _ <- char ':'
                   sss <- secondParser
                   when (hhh == 24 && (mmm /= 0 || sss /= 0))
                     $ fail "invalid time, past 24:00:00"
                   o <- parseOffset
                   return $ mkDateTime yy mm dd hhh mmm sss o

-- | Parse timezone offset.
parseOffset :: Parser (Maybe Pico)
parseOffset = (A.endOfInput *> pure Nothing)
               <|>
               (char 'Z' *> pure (Just 0))
               <|>
               (do sign <- (char '+' *> pure 1) <|> (char '-' *> pure (-1))
                   hh <- fromIntegral <$> p2imax 14
                   _ <- char ':'
                   mm <- fromIntegral <$> p2imax (if hh == 14 then 0 else 59)
                   return . Just $ sign * (hh * 3600 + mm * 60))

yearParser :: Parser Integer
yearParser = do sign <- (char '-' *> pure (-1)) <|> pure 1
                ds <- A.takeWhile isDigit
                when (T.length ds < 4)
                  $ fail "need at least four digits in year"
                when (T.length ds > 4 && T.head ds == '0')
                  $ fail "leading zero in year"
                let Right (absyear, _) = TR.decimal ds
                when (absyear == 0)
                  $ fail "year zero disallowed"
                return $ sign * absyear

secondParser :: Parser Pico
secondParser = do d1 <- digit
                  d2 <- digit
                  frac <- readFrac <$> (char '.' *> A.takeWhile isDigit)
                          <|> pure 0
                  return (read [d1, d2] + frac)
  where
    readFrac ds = read $ '0' : '.' : T.unpack ds

p2imax :: Int -> Parser Int
p2imax m = do a <- digit
              b <- digit
              let n = 10 * val a + val b
              if n > m
                then fail $ "value " ++ show n ++ " exceeded maximum " ++ show m
                else return n
  where
    val c = ord c - ord '0'
