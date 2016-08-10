module Main where

main :: IO ()
main = do
  putStrLn "hello world"


-- Exercise 1 Hopscotch
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
skips :: [a] -> [[a]]
skips []        = []
skips xs@(y:ys) = [xs] ++ (skips ys)

testSkips :: [a] -> Bool
testSkips xs = length xs == (length.skips $ xs)



-- Exercise 2 Local maxima
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: (Ord a) => [a] -> [a]
localMaxima (x:y:z:ss)
  | maximum [x,y,z] == y  = [y] ++ localMaxima (z:ss)
  | otherwise             = localMaxima (y:z:ss)
localMaxima _             = []


-- Exercise 3 Histogram
-- histogram [1,1,1,5] ==
-- *
-- *
-- * *
-- ==========
-- 0123456789
-- histogram [1,4,5,4,6,6,3,4,2,4,9] ==
-- *
-- *
-- * *
-- ****** *
-- ==========
-- 0123456789

histogram :: [Int] -> String
histogram as =
  let
    count i             = length.filter(==i)
    bs                  = [count i as | i <- [0..9] ]
    printRow :: Int -> Int -> String -> String
    printRow x i xs
      | x <= i          = "*" ++ xs
      | otherwise       = " " ++ xs
  in
    foldr (\x xs -> "\n" ++ (foldr (printRow $ x) xs  bs) )  "" (reverse [1..maximum bs])
      ++ "\n=========="
      ++ "\n0123456789"

