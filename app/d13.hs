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

parse = paragraphs . lines

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

solve grid = fmap fst $ find (go . snd) $ indexed $ zippers grid
  where
    go ([],_) = False
    go (_,[]) = False
    go (before,after) = and (zipWith (==) before after)

go grid = eitherA (solve grid) (solve (transpose grid))

summarise = either (* 100) id

one inp = traverse go inp'
  where
    inp' = fmap (fmap equivalenceclasses) inp

two inp = 1


main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp

