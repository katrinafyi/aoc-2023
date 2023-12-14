{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import AocLib
import Control.Applicative
import Control.Arrow hiding (second)
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
import Data.Tuple (swap)
import Debug.Trace
import GHC.Stack (HasCallStack)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P
import Data.Ord
import qualified Data.Map.Merge.Strict as Map

parse = fmap pars . paragraphs . lines
  where
    pars :: [String] -> Map Vec2 Bool
    pars = Map.fromList . concat . indexed2 . fmap (fmap (== '#'))

equivalenceclasses :: (Ord a, Functor f, Foldable f) => f a -> f Integer
equivalenceclasses xs = fmap (mapping !) xs
  where
    mapping = snd $ foldr go (0, Map.empty) xs
    go val (n, map)
      | val `Map.member` map = (n, map)
      | otherwise =  (n+1, Map.insert val n map)

zippers xs = go (xs) []
  where
    go (x:xs) ys = (x:xs, ys) : go xs (x:ys)
    go xs ys = [(xs,ys)]

butheadlast = tail . init

-- n is number of rows before mirror.
mirror grid n = Map.mapKeys k grid
  where
    k (r,c) = (-r + 2 * n + 1, c)
    Just ((maxrow,_),_) = Map.lookupMax grid


zipWithMap f = Map.merge Map.dropMissing Map.dropMissing $ Map.zipWithMatched (const f)

mirrorfold grid n = zipWithMap (==) grid grid'
  where
    grid' = mirror grid n

isfold grid = not (null grid) && and grid

almostfold grid = 2 == sum (Map.map (fromEnum . not) grid)

solve p grid = find (p . snd) $ fmap (id &&& mirrorfold grid) [0..maxrow]
  where
    Just ((maxrow,_),_) = Map.lookupMax grid

solve1 grid = eitherA (s grid) (s (Map.mapKeys swap grid))
  where s = fmap fst . solve isfold

summarise = either (* 100) id

one inp = sum . fmap summarise <$> traverse solve1 inp

two inp = solve almostfold $ (Map.mapKeys id $ head inp)



main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp

