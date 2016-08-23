module Main where

import Lib

main :: IO ()
main = someFunc


-- Exercise 1

fib :: Integer -> Integer
fib 0         = 0
fib 1         = 1
fib 2         = 1
fib x         = fib (x-2) + fib (x-1)

fibs1 :: [Integer]
fibs1         = [fib x| x<-[0..]]

-- Exercise 2
fibs2 :: [Integer]
fibs2         = [0,1] ++ fibs2' [0,1]
  where
    fibs2' :: [Integer] -> [Integer]
    fibs2' xs       =
      let
        next        = sum $ take 2 $ reverse xs
        newfibs2    = xs ++ [next]
      in
        [next] ++ fibs2' newfibs2


-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a c) = a : streamToList c

instance Show a => Show (Stream a) where
  show a = show $ take 20 $ streamToList a

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a    = Cons a (streamRepeat a)

streamMap :: (a->b) -> Stream a -> Stream b
streamMap f (Cons a x) = Cons (f a) (streamMap f x)

streamFromSeed :: (a->a) -> a -> Stream a
streamFromSeed f a = Cons (f a) (streamFromSeed f (f a) )

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) $ -1

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons x xs) ys = Cons x $ interleaveStream ys xs

ruler :: Stream Integer
ruler     = rulerStart 0
    where
      rulerStart :: Integer -> Stream Integer
      rulerStart y    = interleaveStream (streamRepeat y) (rulerStart $ y+1)

