module MuRecursion
  ( proj
  , null
  , succ
  , (<@>)
  , pr
  , Mu
  , runMu
  , runMuIO
  , µ
  ) where

import qualified Control.Arrow as Data.Bifunctor
import Data.Either (either, partitionEithers)
import Data.List (intercalate)
import Prelude hiding (null, succ)
import qualified Prelude as P

newtype Mu =
  Mu
    { unMu :: Log -> [Int] -> Either String (Log, Int)
    }

newtype Log =
  Log [String]

newLog :: Log
newLog = Log []

addLog :: Log -> String -> Log
addLog l@(Log s) s'
  | P.null s' = l
  | otherwise = Log $ s ++ [s']

joinLog :: Log -> Log -> Log
joinLog l (Log s') = foldl addLog l s'

indentLog :: Log -> Char -> Log
indentLog (Log s) c = Log $ map ((c : " ") ++) s

instance Show Log where
  show (Log s) = unlines $ filter (not . P.null) s

runMu :: Mu -> [Int] -> Either String (String, Int)
runMu m args = Data.Bifunctor.first show <$> unMu m newLog args

runMuIO :: Mu -> [Int] -> IO ()
runMuIO m args =
  either
    putStrLn
    (\(a, b) ->
       putStr a >> putStrLn (replicate 5 '-') >>
       putStrLn ("Result is " ++ show b))
    (runMu m args)

proj :: Int -> Int -> Mu
proj k i =
  Mu $ \log args ->
    if length args /= k
      then Left $
           "Argument count mismatch! Proj received " ++
           show (length args) ++ " arguments, but expected " ++ show k
      else Right
             ( addLog log $
               "proj_{" ++ show k ++ " -> " ++ show i ++ "}" ++ fmtArgs args
             , (0 : args) !! i)

null :: Int -> Mu
null k =
  Mu $ \log args ->
    if length args /= k
      then Left $
           "Argument count mismatch! Null received " ++
           show (length args) ++ " arguments, but expected " ++ show k
      else Right (addLog log $ "null_" ++ show k ++ fmtArgs args, 0)

succ :: Mu
succ =
  Mu $ \log args ->
    if length args /= 1
      then Left $
           "Argument count mismatch! Succ received " ++
           show (length args) ++ " arguments, but expected 1"
      else Right (addLog log $ "succ" ++ fmtArgs args, head args + 1)

compose, (<@>) :: Mu -> [Mu] -> Mu
compose g hs =
  Mu $ \log args ->
    let (errs, unMus) =
          partitionEithers $
          map
            (\m ->
               (\(l, r) -> (addLog l $ "=" ++ show r, r)) <$> unMu m newLog args)
            hs
        log' = foldl joinLog newLog $ map fst unMus
        results = map snd unMus
     in if P.null errs
          then unMu
                 g
                 (joinLog
                    (addLog log $ "compose" ++ fmtArgs args)
                    (indentLog log' '#'))
                 results
          else Left $ unlines errs

a <@> b = compose a b

pr :: (Mu, Mu) -> Mu
pr (g, h) = Mu f
  where
    nestL = \(l, r) -> (addLog l $ "=" ++ show r, r)
    f log args
      | last args == 0 = nestL <$> unMu g log (init args)
      | otherwise = do
        (log', f') <- f newLog (init args ++ [last args - 1])
        (\(l, r) -> (addLog l $ "=" ++ show r, r)) <$>
          unMu
            h
            (joinLog (addLog log $ "pr" ++ fmtArgs args) $ indentLog log' '|')
            (f' : init args ++ [last args - 1])

µ :: Mu -> Mu
µ g = Mu $ findMin 0
  where
    findMin :: Int -> Log -> [Int] -> Either String (Log, Int)
    findMin acc log args = do
      (log', r) <-
        unMu g (addLog log $ "µ" ++ fmtArgs (args ++ [acc])) (args ++ [acc])
      if r /= 0
        then findMin (acc + 1) log' args
        else pure (log', acc)

fmtArgs :: [Int] -> String
fmtArgs args = "(" ++ intercalate ", " (map show args) ++ ")"
