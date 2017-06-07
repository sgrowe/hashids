import Test.HUnit
import Test.QuickCheck
import Hashids
import Data.Maybe (fromJust)

(e, d) = fromJust $ hashids defaultHashidSettings
f = fst . fromJust . hashids $ defaultHashidSettings {alphabet = "0123456789abcdef"}
g = fst . fromJust . hashids $ defaultHashidSettings {minLength = 4}
h = fst . fromJust . hashids $ defaultHashidSettings {minLength = 20}
i = fst . fromJust . hashids $ defaultHashidSettings {salt = "This is a salt! :^D", minLength = 50}
j = fst . fromJust . hashids $ defaultHashidSettings {salt = "arbitrary salt", minLength = 16,
  alphabet = "abcdefghijkl0123456"}

testAllNumbers xs
  | all (>= 0) xs = (d . e $ xs) == xs
  | otherwise = null $ e xs

testPositiveNumbers xs = (d . e $ ys) == ys
  where
    ys = map abs xs

testStrings s
  | not . null $ d s = (e . d $ s) == s
  | otherwise = True

testMinLength minLength xs
  | encodedLength == 0 = True
  | otherwise = encodedLength >= minLength'
  where
    ys = map abs xs
    minLength' = abs minLength
    encode = fst . fromJust . hashids $ defaultHashidSettings {minLength = minLength'}
    encodedLength = length $ encode ys

quicktests x = do
  quickCheckWith stdArgs { maxSuccess = x } testAllNumbers
  quickCheckWith stdArgs { maxSuccess = x } testPositiveNumbers
  quickCheckWith stdArgs { maxSuccess = x } testStrings
  quickCheckWith stdArgs { maxSuccess = x } testMinLength

unitTests = test [
  "test1" ~: "Encode 5, 7, 32" ~: e [5] ~=? "nR",
  "test2" ~: "Encode 123" ~: e [123] ~=? "Mj3",
  "test3" ~: "Encode 123, 456, 789" ~: e [123, 456, 789] ~=? "El3fkRIo3",
  "test4" ~: "Encode 0" ~: e [0] ~=? "gY",
  "test5" ~: "Encode -5" ~: e [-5] ~=? "",
  "test6" ~: "Encode 27, 6" ~: f [27, 6] ~=? "36913",
  "test7" ~: "Encode 3" ~: f [3] ~=? "68",
  "test8" ~: "Encode (with min length) 1" ~: g [1] ~=? "ejRe",
  "test9" ~: "Encode (with min length) 7" ~: g [7] ~=? "ep2b",
  "test10" ~: "Encode (with min length) 123 " ~: g [123] ~=? "aMj3",
  "test11" ~: "Encode (with min length) 2, 3, 1 " ~: g [2, 3, 1] ~=? "oYhDi1",
  "test12" ~: "Encode (with min length) 10 " ~: h [10] ~=? "w58DKgl9avmeG1vzAp3E",
  "test13" ~: "Encode (with min length and salt) 4, 10, 120 " ~: i [4, 10, 120] ~=? "gvQzeOGLnKqbajPA471NZkZxhWu7XJyr0W9YRmDB8pdV56Mwox",
  "test14" ~: "Encode (with min length and salt) " ~: j [1, 23, 456] ~=? "1d6216i30h53elk3",
  "test15" ~: "Decode xoz" ~: d "xoz" ~=? [456],
  "test16" ~: "Decode 1B8UvJfXm" ~: d "1B8UvJfXm" ~=? [517, 729, 185]
  ]

main :: IO ()
main = do
    runTestTT unitTests
    quicktests 1000

