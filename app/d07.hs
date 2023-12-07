{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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

data Suit =
  -- strongest first
  SA | SK | SQ | SJ | ST | S9 | S8 | S7 | S6 | S5 | S4 | S3 | S2 | SJoker
  deriving (Eq, Ord, Show, Read)

parse :: String -> [(String, Integer)]
parse raw = fmap (Data.Bifunctor.second read) $ fmap (exactly2 . split ' ') $ lines raw

frequencies xs = Map.fromListWith (+) $ fmap (,1) xs

rank freqmap
  -- strongest is smallest integer
  | max == 5 = 10
  | max == 4 = 20
  | freqs == [2,3] = 30
  | freqs == [1,1,3] = 40
  | freqs == [1,2,2] = 50
  | freqs == [1,1,1,2] = 60
  | freqs == [1,1,1,1,1] = 70
  | otherwise = error $ "unhandled freqs: " ++ show freqs
  where
    freqs = sort $ Map.elems freqmap
    max = maximum freqs
    min = minimum freqs

rank1 hand = rank $ frequencies hand

rank2 hand
  | Map.null freqs = rank $ Map.singleton 'A' jokers
  | otherwise = rank $ Map.adjust (+ jokers) maxvalkey freqs
  -- it is always beneficial to increment the most frequent card
  where
    jokers = length hand - sum freqs
    freqs = frequencies $ filter (/= 'J') hand
    maxvalkey = fst $ maximumBy (compare `on` snd) $ Map.toList freqs


score = sum . zipWith (*) [1..]

solve joking inp = score $ fmap snd $ sortOn (Down . key) inp
  where
    key (hand,_) = (rank hand, suits hand :: [Suit])
    rank = if joking then rank2 else rank1

    suits hand = (\x -> read $ "S" ++ joke x) <$> hand
    joke c = if joking && c == 'J' then "Joker" else [c]

one = solve False

two = solve True

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp
