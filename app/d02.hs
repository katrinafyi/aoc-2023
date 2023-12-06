{-# LANGUAGE OverloadedStrings #-}

import AocLib
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
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
import Data.Coerce (coerce)

type Turn = ZipList Integer
data Game = Game { gameId :: Integer, gameTurns :: [Turn] } deriving (Eq, Show, Read)

parse = parsep

parsep = readp (P.sepBy parser "\n")

parsergb 'r' = pure $ ZipList [1,0,0]
parsergb 'g' = pure $ ZipList [0,1,0]
parsergb 'b' = pure $ ZipList [0,0,1]
parsergb _ = empty

parser = do
  i <- "Game " *> uinteger <* ": "
  a <- flip P.sepBy "; " $ do
    nums <- flip P.sepBy ", " $ do
      x <- uinteger <* " "
      s <- P.munch isLetter
      let c = head s
      fmap (x *) <$> parsergb c
    pure $ fmap sum $ sequenceA nums 
  pure $ Game i a


zipWithMap f = Map.merge Map.dropMissing Map.dropMissing $ Map.zipWithMatched (const f)

one inp = sum $ fmap gameId $ filter (sat . gameTurns) inp
  where
    bound = [12,13,14]
    sat ms = and $ liftA2 (<=) m (ZipList bound)
      where m = maximum <$> sequenceA ms

two inp = sum $ fmap (product) $ fmap needed inp
  where
    needed row = maximum <$> sequenceA (gameTurns row)

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  -- print $ length inp
  -- print $ length $ nub $ toList inp
  print $ one inp
  print $ two inp
