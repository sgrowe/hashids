{-# LANGUAGE NamedFieldPuns #-}

module Hashids (hashids, Hashid, HashidSettings(..), defaultHashidSettings) where

import qualified Data.Vector.Unboxed as V
import Data.Maybe (fromMaybe)
import Control.Monad (zipWithM)
import Data.Char (ord)
import Data.List (nub, foldl')

data HashidSettings = HashidSettings {
  alphabet :: String,
  salt :: String,
  minLength :: Int
  } deriving (Show)

defaultHashidSettings :: HashidSettings
defaultHashidSettings = HashidSettings {
  alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
  salt = "",
  minLength = 0
  }

type Hashid = String

type Alphabet = V.Vector Char
type Salt = V.Vector Char
type MinLength = Int
type Seperators = V.Vector Char
type Guards = V.Vector Char

type HashidConfig = (Alphabet, Salt, MinLength, Seperators, Guards)

--type HashidConfig = (V.Vector Char, V.Vector Char, Int, V.Vector Char, V.Vector Char)
-- config order: (alphabet, salt, minlength, seperators, guards)

seperatorChars :: V.Vector Char
seperatorChars = V.fromList "cfhistuCFHISTU"

seperatorsRatio :: Rational
seperatorsRatio = 7 / 2

guardsRatio :: Rational
guardsRatio = 12

modBy :: Integral a => a -> a -> a
modBy = flip mod

switch :: V.Vector Char -> (Int, Int) -> V.Vector Char
switch alphabet (i, j) = alphabet `V.unsafeUpd` [(i, letterAtj), (j, letterAti)]
  where
    letterAti = alphabet V.! i
    letterAtj = alphabet V.! j

shuffle :: V.Vector Char -> V.Vector Char -> V.Vector Char
shuffle alphabet salt
  | V.null salt = alphabet
  | otherwise = foldl' switch alphabet pairsOfIndices
  where
    add3 a b c = a + b + c
    indices = reverse [1 .. V.length alphabet - 1]
    saltIndices = cycle [0 .. V.length salt - 1]
    saltOrds = cycle . V.toList . V.map ord $ salt
    largeNumbersFromSalt = zipWith3 add3 saltIndices saltOrds . scanl1 (+) $ saltOrds
    pairsOfIndices = zip indices . zipWith modBy indices $ largeNumbersFromSalt

shuffledAlphabets :: V.Vector Char -> Char -> V.Vector Char -> [V.Vector Char]
shuffledAlphabets alphabet lotteryChar salt = tail . iterate superShuffle $ alphabet
  where
    superShuffle alpha = shuffle alpha . V.take (V.length alphabet) $ lotteryChar `V.cons` salt V.++ alpha

ensureNotEmpty :: [Int] -> [Int]
ensureNotEmpty [] = [0]
ensureNotEmpty xs = xs

toHex :: V.Vector Char -> Int -> V.Vector Char
toHex alphabet input = V.unsafeBackpermute alphabet letterIndices
  where
    quotients = ensureNotEmpty . takeWhile (> 0) . iterate (`quot` V.length alphabet) $ input
    letterIndices = V.reverse . V.map (`mod` V.length alphabet) . V.fromList $ quotients

fromHex :: V.Vector Char -> V.Vector Char -> Maybe Int
fromHex alphabet hash = do
  positions <- V.mapM (`V.elemIndex` alphabet) hash
  return . V.sum . V.imap f $ positions
  where
    f index position = position * (V.length alphabet ^ (V.length hash - index - 1))

ensureLength :: V.Vector Char -> Int -> V.Vector Char -> Int -> V.Vector Char -> V.Vector Char
ensureLength alphabet minLength guards valuesHash encoded
  | V.length encoded >= minLength = encoded
  | V.length encoded + 1 < minLength = ensureLength' alphabet minLength encodedTwoExtra
  | otherwise = ensureLength' alphabet minLength encodedOneExtra
  where
    guardIndex x xs = modBy (V.length guards) $ valuesHash + ord (xs V.! x)
    encodedOneExtra = (guards V.! guardIndex 0 encoded) `V.cons` encoded
    encodedTwoExtra = encodedOneExtra `V.snoc` (guards V.! guardIndex 2 encodedOneExtra)

ensureLength' :: V.Vector Char -> Int -> V.Vector Char -> V.Vector Char
ensureLength' alpha minLength hashid
  | V.length hashid < minLength = ensureLength' alpha minLength hashid''
  | otherwise = hashid
  where
    splitIndex = V.length alpha `quot` 2
    (firstPart, lastPart) = V.splitAt splitIndex $ shuffle alpha alpha
    hashid' = V.concat [lastPart, hashid, firstPart]
    excess = V.length hashid' - minLength
    hashid'' = V.take minLength . V.drop (excess `quot` 2) $ hashid'

safeIndex :: V.Vector Char -> Int -> Char
safeIndex xs x = xs V.! (x `mod` V.length xs)

encode :: HashidConfig -> [Int] -> Hashid
encode (alphabet, salt, minLength, seperators, guards) values
  | all (>= 0) values && not (null values) = V.toList . ensureLengthCurried $ hashid
  | otherwise = []
  where
    valuesHash = sum . zipWith modBy [100..] $ values
    lotteryChar = alphabet `safeIndex` valuesHash
    alphabets = shuffledAlphabets alphabet lotteryChar salt
    hashid = V.init . V.cons lotteryChar . V.concat . zipWith3 (encoder seperators) [0..] alphabets $ values
    newAlphabet = alphabets !! (length values - 1)
    ensureLengthCurried = ensureLength newAlphabet minLength guards valuesHash

encoder seperators i alphabet' value = valueHexed `V.snoc` seperator
  where
    valueHexed = toHex alphabet' value
    seperator = safeIndex seperators . mod value $ i + ord (V.head valueHexed)

-- Rewrite without recursion?
splitOn :: V.Vector Char -> V.Vector Char -> [V.Vector Char]
splitOn hashid splitters
  | V.null hashid = []
  | otherwise = start : safeTail theRest `splitOn` splitters
  where
    safeTail = V.drop 1
    (start, theRest) = V.break (`V.elem` splitters) hashid

decode :: HashidConfig -> Hashid -> [Int]
decode config@(alphabet, salt, _, seperators, guards) hashid
  | null hashid || V.null hashid' = []
  | encode config decoded == hashid = decoded
  | otherwise = []
  where
    parts = V.fromList hashid `splitOn` guards
    hashid' = if length parts == 2 || length parts == 3 then parts !! 1 else head parts
    lotteryChar = V.head hashid'
    hashParts = V.tail hashid' `splitOn` seperators
    alphabets = shuffledAlphabets alphabet lotteryChar salt
    decoded = fromMaybe [] $ zipWithM fromHex alphabets hashParts

indexFromRatio :: (Real a, Integral b) => a -> Rational -> b
indexFromRatio x y = ceiling $ toRational x / y

getConfig :: HashidSettings -> Maybe HashidConfig
getConfig HashidSettings {alphabet, salt, minLength}
  | V.length characters < 16 = Nothing
  | otherwise = Just (alpha', salt', minLength, seps, guards)
  where
    salt' = V.fromList salt
    characters = V.fromList $ nub alphabet
    (seperators, alphaChars) = V.partition (`V.elem` seperatorChars) characters
    sepsNeeded = max 2 (indexFromRatio (V.length alphaChars) seperatorsRatio) - V.length seperators
    (extraSeperators, reducedAlphabet) = V.splitAt sepsNeeded alphaChars
    seperators' = shuffle seperators salt' V.++ extraSeperators
    alpha = shuffle reducedAlphabet salt'
    numGuards = indexFromRatio (V.length alpha) guardsRatio
    splitSeps = V.splitAt numGuards seperators'
    splitAlpha = V.splitAt numGuards alpha
    (guards, seps, alpha') = if V.length reducedAlphabet < 3
      then (fst splitSeps, snd splitSeps, alpha)
      else (fst splitAlpha, seperators', snd splitAlpha)

hashids :: HashidSettings -> Maybe ([Int] -> Hashid, Hashid -> [Int])
hashids settings = do
  config <- getConfig settings
  return (encode config, decode config)

