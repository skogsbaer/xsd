{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

module Text.XML.XSD
       ( E18
       , Atto
       , boolean
       , toBoolean
       , decimal
       , integer
       , toSigned
       , long
       , int
       , short
       , byte
       , unsignedInteger
       , unsignedLong
       , unsignedInt
       , unsignedShort
       , unsignedByte
       , float
       , double
       , fastDouble
       , module Text.XML.XSD.DateTime
       ) where

import           Text.XML.XSD.DateTime

import           Control.Monad (when)
import           Data.Fixed (Fixed, HasResolution(..), resolution)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Read as TR
import           Data.Word (Word8, Word16, Word32, Word64)

data E18

instance HasResolution E18 where
  resolution _ = 1000000000000000000

type Atto = Fixed E18

checkReader :: Either String (a, Text) -> Either String a
checkReader res =
  do (val, t) <- res
     when (t /= "") $ Left "leftovers"
     return val

-- checkReader (Left msg) = Left msg
-- checkReader (Right (val, t))
--   | t /= "" = Left "leftovers"
--   | otherwise = Right val

boolean :: Text -> Either String Bool
boolean t =
  if t == "true" || t == "1"
  then Right True
  else if t == "false" || t == "0"
       then Right False
       else Left $ "invalid boolean '" ++ T.unpack t ++ "'"

toBoolean :: Bool -> Text
toBoolean True = "true"
toBoolean False = "false"

decimal :: Text -> Either String Atto
decimal = checkReader . TR.rational

signed :: Integral a => Text -> Either String a
signed = checkReader . TR.signed TR.decimal

unsigned :: Integral a => Text -> Either String a
unsigned = checkReader . TR.decimal

toSigned :: Integral a => a -> Text
toSigned = TL.toStrict . TB.toLazyText . TBI.decimal

integer :: Integral a => Text -> Either String a
integer = signed

long :: Text -> Either String Int64
long = signed

int :: Text -> Either String Int32
int = signed

short :: Text -> Either String Int16
short = signed

byte :: Text -> Either String Int8
byte = signed

-- | What XSD calls @nonNegativeInteger@.
unsignedInteger :: Integral a => Text -> Either String a
unsignedInteger = unsigned

unsignedLong :: Text -> Either String Word64
unsignedLong = unsigned

unsignedInt :: Text -> Either String Word32
unsignedInt = unsigned

unsignedShort :: Text -> Either String Word16
unsignedShort = unsigned

unsignedByte :: Text -> Either String Word8
unsignedByte = unsigned

float :: Text -> Either String Float
float = checkReader . TR.rational

double :: Text -> Either String Double
double = checkReader . TR.rational

fastDouble :: Text -> Either String Double
fastDouble = checkReader . TR.double
