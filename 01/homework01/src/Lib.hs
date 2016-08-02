module Lib
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate
    , hanoi
    ) where


-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits i | i <= 0     = []
           | otherwise  = toDigits  (i `div` 10) ++ [i `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse.toDigits

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse [sum | (x,i) <- zip (reverse xs) [0..]
                           , let sum = x + (i `mod` 2) * x ]

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ fmap sumIndivDigits xs
          where
            sumIndivDigits = (sum.toDigits)


-- Exercise 4
validate :: Integer -> Bool
validate  = let isValidChecksum = (==) 0
          in
            isValidChecksum.(flip mod 10).sumDigits.doubleEveryOther.toDigits


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

{--
  hanoi 2 "a" "b" "c" == [("a","b"),("a","c"),("b","c")]
--}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = moveTower n a c b []
              where moveTower :: Integer -> Peg -> Peg -> Peg -> [Move] -> [Move]
                    moveTower 0 _ _ _ ms        = ms
                    moveTower n from to with ms = ms
                                                  ++ moveTower (n-1) from with to ms
                                                  ++ [(from, to)]
                                                  ++ moveTower (n-1) with to from ms


-- TODO: When you learn Reader
--displayHanoi :: [Move] -> IO ()

