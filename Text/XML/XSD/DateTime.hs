-- | XSD @dateTime@ data structure <http://www.w3.org/TR/xmlschema-2/#dateTime>
module Text.XML.XSD.DateTime(
                              DateTime,
                              dateTime',
                              dateTime,
                              toZonedTime,
                              fromZonedTime,
                              zonedTime',
                              zonedTime,
                              toUTCTime,
                              fromUTCTime,
                              utcTime',
                              utcTime
                            ) where

import Text.ParserCombinators.Parsec
import Data.Ord
import Data.Maybe
import Data.Time
import Data.Function
import Control.Monad
import Control.Arrow

-- | XSD @dateTime@ data structure <http://www.w3.org/TR/xmlschema-2/#dateTime>
data DateTime = DateTime Bool Int Int Int Int Int Int (Maybe String) Offset

instance Show DateTime where
  show (DateTime neg yy mm dd hhh mmm sss ssss tz) =
    join [if neg then "-" else [], showy yy, "-", showi mm, "-", showi dd, "T", showi hhh, ":", showi mmm, ":", showi sss, seconds ssss,show tz]

instance Read DateTime where
  readList s = [(maybeToList (dateTime s), [])]

instance Eq DateTime where
  (==) = (==) `on` (zonedTimeToLocalTime . toZonedTime &&& zonedTimeZone . toZonedTime)

instance Ord DateTime where
  compare = comparing (zonedTimeToLocalTime . toZonedTime &&& zonedTimeZone . toZonedTime)

-- | Parses the string into a @dateTime@ or may fail with a parse error.
dateTime' :: String -> Either ParseError DateTime
dateTime' = parse parseDateTime "DateTime parser"

-- | Parses the string into a @dateTime@ or may fail.
dateTime :: String -> Maybe DateTime
dateTime = either (const Nothing) Just . dateTime'

-- | Converts a @dateTime@ to a zoned time.
toZonedTime :: DateTime -> ZonedTime
toZonedTime (DateTime neg yy mm dd hhh mmm sss ssss tz) =
  ZonedTime (
    LocalTime (fromGregorian (fromIntegral ((if neg then negate else id) yy)) mm dd) (
    TimeOfDay hhh mmm (realToFrac (read (show sss ++ seconds ssss) :: Double)))) (timeZone tz)

-- | Converts a zoned time to a @dateTime@.
fromZonedTime :: ZonedTime -> DateTime
fromZonedTime (ZonedTime (LocalTime d (TimeOfDay hhh mmm sss)) (TimeZone m _ _)) =
  let (yy, mm, dd) = toGregorian d
      (sss1, sss2) = properFraction sss
      (hz, mz) = m `quotRem` 60
  in DateTime (yy < 0) (abs (fromIntegral yy)) mm dd hhh mmm sss1 (Just (trimTail (== '0') (show sss2))) (Offset False (Just (hz < 0)) (Just hz) (Just mz))

-- | Parses the string in a @dateTime@ then converts to a zoned time and may fail with a parse error.
zonedTime' :: String -> Either ParseError ZonedTime
zonedTime' = fmap toZonedTime . dateTime'

-- | Parses the string in a @dateTime@ then converts to a zoned time and may fail.
zonedTime :: String -> Maybe ZonedTime
zonedTime = fmap toZonedTime . dateTime

-- | Converts a @dateTime@ to a UTC time.
toUTCTime :: DateTime -> UTCTime
toUTCTime = uncurry localTimeToUTC . (zonedTimeZone &&& zonedTimeToLocalTime) . toZonedTime

-- | Converts a UTC time to a @dateTime@.
fromUTCTime :: UTCTime -> DateTime
fromUTCTime (UTCTime d t) =
  let (yy, mm, dd) = toGregorian d
      TimeOfDay hhh mmm sss = timeToTimeOfDay t
      (sss1, sss2) = properFraction sss
  in DateTime (yy < 0) (abs (fromIntegral yy)) mm dd hhh mmm sss1 (Just (trimTail (== '0') (show sss2))) (Offset True Nothing Nothing Nothing)

-- | Parses the string in a @dateTime@ then converts to a UTC time and may fail with a parse error.
utcTime' :: String -> Either ParseError UTCTime
utcTime' = fmap toUTCTime . dateTime'

-- | Parses the string in a @dateTime@ then converts to a UTC time and may fail.
utcTime :: String -> Maybe UTCTime
utcTime = fmap toUTCTime . dateTime

-- not exported

data Offset = Offset Bool (Maybe Bool) (Maybe Int) (Maybe Int)
  deriving Eq

instance Show Offset where
  show (Offset False Nothing Nothing Nothing) = []
  show (Offset True Nothing Nothing Nothing) = "Z"
  show (Offset False (Just neg) (Just hh) (Just mm)) = join [if neg then "-" else "+", showi hh, ":", showi mm]
  show _ = error "Offset invariant not met"

timeZone :: Offset -> TimeZone
timeZone (Offset False Nothing Nothing Nothing) = TimeZone 0 False "undetermined"
timeZone (Offset True Nothing Nothing Nothing) = TimeZone 0 False "UTC"
timeZone (Offset False (Just neg) (Just hh) (Just mm)) = TimeZone ((if neg then negate else id) hh * 60 + mm) False (join ["UTC", if neg then "-" else "+", showi hh, ":", showi mm])
timeZone _ = error "Offset invariant not met"

seconds :: Maybe String -> String
seconds (Just d) = '.' : d
seconds Nothing = []

showi :: (Num a, Ord a) => a -> String
showi n = (if n < 10 then ('0':) else id) (show n)

showy :: (Num a, Ord a) => a -> String
showy n = let k t = if n < t then ('0':) else id
          in k 1000 (k 100 (k 10 (show n)))

parseOffset :: GenParser Char st Offset
parseOffset = let e = const (Offset False Nothing Nothing Nothing) `fmap` eof
                  z = const (Offset True Nothing Nothing Nothing) `fmap` char 'Z'
                  o = do neg <- fmap (== '-') (char '+' <|> char '-')
                         hh <- p2imax 14
                         char ':'
                         mm <- p2imax (if hh == 14 then 0 else 59)
                         return (Offset False (Just neg) (Just hh) (Just mm))
              in e <|> z <|> o

parseDateTime :: GenParser Char st DateTime
parseDateTime = do neg <- isJust `fmap` optionMaybe (char '-')
                   yy <- yearParser
                   char '-'
                   mm <- p2imax 12
                   char '-'
                   dd <- p2imax ([31, if isLeapYear (fromIntegral mm) then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (mm - 1))
                   char 'T'
                   hhh <- p2imax 23
                   char ':'
                   mmm <- p2imax 59
                   char ':'
                   sss <- p2imax 59
                   ssss <- optionMaybe fractionalSeconds
                   o <- parseOffset
                   return (DateTime neg yy mm dd hhh mmm sss ssss o)

yearParser :: GenParser Char st Int
yearParser = do d1 <- digit
                d2 <- digit
                d3 <- digit
                d4 <- digit
                ds <- many digit
                if not (null ds) && d1 == '0'
                  then unexpected "leading zero in year"
                  else return (read ([d1, d2, d3, d4] ++ ds))

fractionalSeconds :: GenParser Char st String
fractionalSeconds = do char '.'
                       many1 digit

p2imax :: Int -> GenParser Char st Int
p2imax m = do a <- digit
              b <- digit
              let n = read [a, b]
              if n > m then unexpected ("value " ++ show n ++ " exceeded maximum " ++ show m) else return n

-- examples = ["2009-10-10T03:10:10-05:00", "2119-10-10T03:10:10.4-15:26", "0009-10-10T03:10:10+15:00", "2009-10-10T03:10:10Z", "-2009-05-10T21:08:59-05:00", "-9399-12-31T13:10:10+15:00", "-2009-10-10T03:10:10Z"]

-- not exported

trimTail :: (a -> Bool) -> [a] -> [a]
trimTail = (reverse .) . (. reverse) . dropWhile
