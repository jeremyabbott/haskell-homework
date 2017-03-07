module Main where

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x < 0 = []
  | otherwise  = toDigits (x `div` 10) ++ [(x `mod` 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs


doubleEveryOther' :: [Integer] -> Bool -> [Integer]
doubleEveryOther' [] b = []
doubleEveryOther' (x:xs) True = 2*x : doubleEveryOther' xs False
doubleEveryOther' (x:xs) False = x : doubleEveryOther' xs True

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = 
  reverse (doubleEveryOther' (reverse x) False)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x > 9 = sumDigits xs + sumDigits (toDigits x)
  | otherwise = x + sumDigits xs

validate :: Integer -> Bool
validate ccNumber =
  sumDigits (doubleEveryOther (toDigits ccNumber)) `mod` 10 == 0

  

main :: IO ()
main = do
  print (toDigitsRev 1234)
