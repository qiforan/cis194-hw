module Lib
    (
     toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate, hanoi
    ) where


-- exercise 1 begin
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
    | otherwise = x `rem` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
-- exercise 1 end


--- exercise 2 begin
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:z) = [x,y * 2] ++ doubleEveryOther' z

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ doubleEveryOther' $ reverse x
--- exercise 2 end

--- exercise 3 begin
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0
--- exercise 3 end

--- exercise 4 begin
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x)) `rem` 10) == 0
--- exercise 4 end


--- exercise 5 begin
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dest other = hanoi (n-1) src other dest ++ [(src, dest)]  ++ hanoi (n-1) other dest src
--- exercise 5 end
