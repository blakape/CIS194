{-# LANGUAGE FlexibleInstances, DeriveAnyClass #-}

module Main where

import Lib
import ExprT as E
import Parser
import StackVM as VM

main :: IO ()
main = someFunc


-- Exercise 1
eval :: ExprT -> Integer
eval (E.Lit x)        = x
eval (E.Add x y)      = (+) (eval x) (eval y)
eval (E.Mul x y)      = (*) (eval x) (eval y)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr ex = do
  expr <- parseExp E.Lit E.Add E.Mul ex
  return (eval expr)


-- Exercise 3
class Expr a where
  lit       :: Expr a => Integer -> a
  add, mul  :: Expr a => a -> a -> a

instance Expr ExprT where
  lit         = E.Lit
  add         = E.Add
  mul         = E.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr Integer where
  lit         = id
  add         = (+)
  mul         = (*)

instance Expr Bool where
  lit         = (>=1)
  add         = (||)
  mul         = (&&)

newtype MinMax = MinMax Integer
            deriving (Show, Eq, Ord)

instance Expr MinMax where
  lit x       = MinMax x
  add         = max
  mul         = min

newtype Mod7 = Mod7 Integer
            deriving (Show, Eq)

mod7 :: Integer -> Mod7
mod7 x       = Mod7 (x `mod` 7)

instance Expr Mod7 where
  lit x       = mod7 x
  add (Mod7 x) (Mod7 y)  = mod7 $ x + y
  mul (Mod7 x) (Mod7 y)  = mod7 $ x * y

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7


-- Exercise 5
instance Expr VM.Program where
  lit x       = [PushI x]
  add x y     = x ++ y ++ [VM.Add]
  mul x y     = x ++ y ++ [VM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul


-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExpT =
  Var String
  deriving (Show)


