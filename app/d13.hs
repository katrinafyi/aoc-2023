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
import Data.Tuple (swap)
import Debug.Trace
import GHC.Stack (HasCallStack)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P
import Data.Ord

pars = map (== '#')

parse = paragraphs . lines

equivalenceclasses :: (Ord a, Functor f, Foldable f) => f a -> f Integer
equivalenceclasses xs = fmap (mapping !) xs
  where 
    mapping = snd $ foldr go (0, Map.empty) xs
    go val (n, map)
      | val `Map.member` map = (n, map)
      | otherwise =  (n+1, Map.insert val n map)

solve grid = (reverse $ fmap swap before, tail after)
  -- | null after = mempty
  -- | ismirror = [length before]
  -- | otherwise = mempty
  where
    (before, after) = break (uncurry (==)) $ sliding2 grid
    ismirror = and $ zipWith (==) (reverse $ fmap swap before) (drop 1 after)

-- solveboth grid = eitherA (solve grid) (solve (transpose grid))

one inp = map solve $ map equivalenceclasses $ map transpose $ inp
two inp = 1


main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp
