{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Wires.SWIFT.Internal
  ( SWIFT(..)
  , SWIFTError(..)
  , parseSWIFT
  , SpecifiedElement
  , country
  , checkPattern
  ) where

import           Control.Arrow (left)
import           Data.Char (digitToInt, isAlphaNum, isDigit, isAsciiLower, isAsciiUpper, toUpper)
import           Data.Map (Map)
import qualified  Data.Map as M
import           Data.ISO3166_CountryCodes (CountryCode)
import           Data.List (foldl')
import           Data.Maybe (isNothing)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified  Data.Text as T
import qualified  Wires.SWIFT.Data as Data
import          Text.Read (Lexeme(Ident), Read(readPrec), parens, prec, readMaybe, readPrec, lexP)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

data SWIFT = SWIFT {rawSWIFT :: Text}
  deriving (Eq)

instance IsString SWIFT where
    fromString swift = either (error . show) id $ parseSWIFT $ T.pack swift

instance Show SWIFT where
    showsPrec p swift = showParen (p>10) $
        showString "fromString " . shows swift

instance Read SWIFT where
    readPrec = parens $ prec 10 $ do
        Ident "fromString" <- lexP
        fromString <$> readPrec

-- | Get the country of the SWIFT
country :: SWIFT -> CountryCode
country = either err id . countryEither . rawSWIFT
  where err = const $ error "SWIFT.country: internal inconsistency"

-- | Parse the Country from a text SWIFT
countryEither :: Text -> Either Text CountryCode
countryEither s = readNote' s $ T.take 2 s

data SWIFTError =
    SWIFTInvalidCharacters   -- ^ The SWIFT string contains invalid characters.
  | SWIFTInvalidStructure    -- ^ The SWIFT string has the wrong structure.
  | SWIFTWrongChecksum       -- ^ The checksum does not match.
  | SWIFTInvalidCountry Text -- ^ The country identifier is either not a
                             --   valid ISO3166-1 identifier or that country
                             --   does not issue SWIFTs.
  deriving (Show, Read, Eq)

data SpecifiedElement = SpecifiedElement (Char -> Bool) Int Bool

type BBANPattern = [SpecifiedElement]

-- | show a SWIFT code in a block pattern.
-- Example: GENODEM1GLS would become GENO DE M1 GLS
prettySWIFT :: SWIFT -> Text
prettySWIFT (SWIFT str) = undefined
-- prettySWIFT (SWIFT str) = T.intercalate " " $ T.chunksOf 4 str

-- | try to parse a SWIFT
parseSWIFT :: Text -> Either SWIFTError SWIFT
parseSWIFT str
  | wrongChars = Left SWIFTInvalidCharacters
  | otherwise = do
                  country' <- left SWIFTInvalidCountry $ countryEither s
                  if checkPattern str
                    then Right $ SWIFT str
                    else Left SWIFTInvalidStructure
  where
    s              = T.filter (/= ' ') str
    wrongChars     = T.any (not . isAlphaNum) s 

checkPattern :: Text -> Bool
checkPattern s = check (Just s)
  where
    check :: Maybe Text -> Bool
    check Nothing = False
    check (Just t) = t =~ swiftRegex :: Bool
    swiftRegex = "^[a-zA-Z]{6}[a-zA-Z0-9]{2}([a-zA-Z0-9]{3})?$"

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

readNote' :: Read a => b -> Text -> Either b a
readNote' n = maybe (Left n) Right . readMaybe . T.unpack
