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
import qualified Data.Map as Map
import Data.Maybe
import Data.Sequence (Seq ((:<|)), (<|), (><), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tuple
import Debug.Trace
import GHC.Stack (HasCallStack)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P

data Data = Data {dataNumbers :: Map Vec2 ([Vec2], Integer), dataSymbols :: Map Vec2 Char} deriving (Eq, Show)

parse raw = Data numbers symbols
  where
    symbols = Map.fromList $ filter (\(_, x) -> not (isDigit x) && x /= '.') $ concat $ indexed2 $ lines raw
    numbers = Map.unions $ fmap makeNumbers $ filterAndGroup (isDigit . snd) $ concat $ indexed2 $ lines raw
    -- makeNumbers :: [(Vec2, Char)] -> Map Vec2 (Set Vec2, Integer)
    makeNumbers inp = Map.fromList $ fmap (,rhs) keys
      where
        rhs = Data.Bifunctor.second (read :: String -> Integer) $ unzip inp
        keys = fst rhs

one (Data nums syms) = sum $ fmap snd $ uniquify $ Map.elems $ filterKeys (`elem` adjs) nums
  where
    adjs = concatMap adjacent8 $ Map.keys syms

two (Data nums syms) = sum $ fmap product $ mapMaybe sat adjs
  where
    adjs = map adjacent8 $ Map.keys $ Map.filter (== '*') syms
    sat adjs = guard (length x == 2) $> fmap snd x
      where
        x = uniquify $ Map.elems $ filterKeys (`elem` adjs) nums

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  -- print $ length inp
  -- print $ length $ nub $ toList inp
  print $ one inp
  print $ two inp
