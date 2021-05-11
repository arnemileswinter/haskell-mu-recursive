module Main where

import MuRecursion
import Prelude hiding (exp, null, pred, succ)

main :: IO ()
main = do
  a <- read <$> getLine
  b <- read <$> getLine
  print $ runMu add [a, b]

add, mult, exp, isNull :: Mu
-- bekannt aus VL
-- add :: Int -> Int -> Int
-- add a b = a + b
add = pr (proj 1 1, succ <@> [proj 3 1])

-- bekannt aus VL
-- mult :: Int -> Int -> Int 
-- mult a b = a*b
mult = pr (null 1, add <@> [proj 3 1, proj 3 2])

-- bekannt aus VL
-- exp :: Int -> Int -> Int
-- exp a b = a ^ b
exp = pr (succ <@> [null 1], mult <@> [proj 3 1, proj 3 2])
