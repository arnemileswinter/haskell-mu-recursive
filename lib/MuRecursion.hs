module MuRecursion
  ( proj
  , null
  , succ
  , (<@>)
  , pr
  , runMu
  , Mu
  , argCount
  ) where

import Data.Either (partitionEithers)
import Data.List (intercalate)
import Prelude hiding (null, succ)
import qualified Prelude as P

type ArgC = Int

data Mu
  = Proj ArgC Int
  | Null ArgC
  | Succ
  | Compose Mu [Mu]
  | PR Mu Mu
  deriving (Eq)

proj = Proj

null = Null

succ = Succ

a <@> b = Compose a b

pr (a, b) = PR a b

k, argCount :: Mu -> Int
argCount = k

k (Proj k' _) = k'
k (Null k') = k'
k Succ = 1
k (Compose g hs) = foldl max 0 (map k hs) -- hs should all have the same k.
k (PR g h) = k g + 1

fmtArgs :: [Int] -> String
fmtArgs args = "(" ++ intercalate ", " (map show args) ++ ")"

runMu :: Mu -> [Int] -> Either String Int
runMu mu args
  | any (< 0) args = Left $ "Received negative number in args " ++ show args
  | length args /= k mu =
    Left $
    "Argument count mismatch! Expected " ++
    show (k mu) ++ " but received " ++ show (length args)
  | otherwise = go mu
  where
    go :: Mu -> Either String Int
    -- primitive definitions
    go (Null _) = pure 0
    go (Proj _ i) = pure $ (0 : args) !! i -- prefix args with 0 such that index starts at 1. 
                                               -- Safe indexing because k implies argument count.
    go Succ = pure $ head args + 1 -- Safe head because k implies argument count.
    -- composition
    go (Compose g hs) =
      let (lefts, rights) = partitionEithers $ map (`runMu` args) hs
       in if P.null lefts
            then runMu g rights
            else Left $ unlines lefts
    -- primitive recursion
    go (PR g h)
      | last args == 0 = runMu g (init args)
      | otherwise = do
        let m' = last args - 1
        r <- runMu mu (init args ++ [m'])
        runMu h (r : init args ++ [m'])
