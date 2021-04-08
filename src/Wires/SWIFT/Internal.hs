{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Wires.SWIFT.Internal
  ( SWIFT(..)
  , SWIFTError(..)
  , parseSWIFT
  , SpecifiedElement
  , country
  , checkPattern
  , parsePattern
  , countryPatterns
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
import           Text.Read (Lexeme(Ident), Read(readPrec), parens, prec, readMaybe, readPrec, lexP)

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
                  pattern <- note (SWIFTInvalidCountry $ T.take 2 s) $
                                    M.lookup country' countryPatterns
                  if checkPattern pattern s
                    then Right $ SWIFT s
                    else Left SWIFTInvalidStructure
  where
    s              = T.filter (not . (== ' ')) str
    wrongChars     = T.any (not . isAlphaNum) s
    -- wrongChecksum  = 1 /= mod97_10 s

checkPattern :: BBANPattern -> Text -> Bool
checkPattern structure s = isNothing $ foldl' step (Just s) structure
  where
    step :: Maybe Text -> SpecifiedElement -> Maybe Text
    step Nothing _ = Nothing
    step (Just t) (SpecifiedElement cond cnt strict) =
      case T.dropWhile cond t' of
        "" -> Just r
        r' -> if strict then Nothing
                        else Just $ r' <> r
      where
        (t', r) = T.splitAt cnt t

parsePattern :: Text -> (CountryCode, BBANPattern)
parsePattern completePattern = (cc, structure)
  where
    (cc', s) = T.splitAt 2 completePattern
    cc = either err id $ readNote' ("invalid country code" <> show cc') cc'

    structure = case T.foldl' step (0, False, []) s of
                  (0, False, xs) -> reverse xs
                  _ -> err "invalid"

    step :: (Int, Bool, [SpecifiedElement]) -> Char -> (Int, Bool, [SpecifiedElement])
    step (_,   True,   _ ) '!' = err "unexpected '!'"
    step (cnt, False,  xs) '!' = (cnt, True, xs)
    step (cnt, strict, xs)  c
      | isDigit c               = (cnt*10 + digitToInt c, False, xs)
      | elem c ("nace"::String) = addElement xs condition cnt strict
      | otherwise               = err $ "unexpected " ++ show c
      where
        condition = case c of
                      'n' -> isDigit
                      'a' -> isAsciiUpper
                      'c' -> \c' -> isAsciiUpper c' || isDigit c'
                      'e' -> (== ' ')

    addElement xs repr cnt strict = (0, False, SpecifiedElement repr cnt strict : xs)
    err details = error $ "SWIFT.parsePattern: " <> details <> " in " <> show s

countryPatterns :: Map CountryCode BBANPattern
countryPatterns = M.fromList $ map parsePattern Data.patterns

-- TODO remove; we don't need a checksum for SWIFT codes
-- | Calculate the reordered decimal number mod 97 using Horner's rule.
-- according to ISO 7064: mod97-10
-- mod97_10 :: Text -> Int
-- mod97_10 = fold . reorder
--   where reorder = uncurry (flip T.append) . T.splitAt 4
--         fold = T.foldl' ((flip rem 97 .) . add) 0
--         add n c
--           -- is this right? all examples in the internet ignore lowercase
--           | isAsciiLower c = add n $ toUpper c
--           | isAsciiUpper c = 100*n + 10 + fromEnum c - fromEnum 'A'
--           | isDigit c      = 10*n + digitToInt c
--           | otherwise      = error $ "SWIFT.Internal.mod97: wrong char " ++ [c]

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

readNote' :: Read a => b -> Text -> Either b a
readNote' n = maybe (Left n) Right . readMaybe . T.unpack
