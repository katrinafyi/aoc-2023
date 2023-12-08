{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import AocLib
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq ((:<|)), (<|), (><), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tuple ()
import Debug.Trace
import GHC.Stack (HasCallStack)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P
import Data.Ord

data Dir = L | R deriving (Eq, Show, Read)

parse raw = (dirs, Map.fromList $ fmap pars rest)
  where
    (dirline, rest) = splitAt 2 $ lines raw
    dirs = read @Dir . pure <$> head dirline
    valid = liftA2 (||) isDigit isLetter
    pars = t . filterAndGroup valid
      where t [a,b,c] = (a,(b,c))

isZ = (== 'Z') . last
isA = (== 'A') . last

whileRight = rights . takeWhile isRight
whileJust = catMaybes . takeWhile isJust

solve end aaa (dirs, edges) = takeWhile (not . end) $ scanl' go aaa (cycle dirs)
  where
    lr L = fst
    lr R = snd
    go node d = lr d $ edges ! node

one = length . solve (== "ZZZ") "AAA"

two (dirs, edges) = foldl' lcm 1 $ genericLength <$> fmap go aaas
  where
    go aaa = solve isZ aaa (dirs, edges)
    aaas = filter isA $ Map.keys edges

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp
