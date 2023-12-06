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


data Colour = R | G | B deriving (Eq, Show, Read, Ord)
type Turn = Map Colour Integer
data Game = Game { gameId :: Integer, gameTurns :: [Turn] } deriving (Eq, Show, Read)

parse = parsep

parsep = readp (P.sepBy parser (P.char '\n'))

parser = do
  P.string "Game "
  i <- uinteger
  P.string ": "
  a <- flip P.sepBy (P.string "; ") $ do
    flip P.sepBy (P.string ", ") $ do
      x <- uinteger
      P.char ' '
      s <- P.munch isLetter
      let c = read $ take 1 $ fmap toUpper s
      pure (c :: Colour, x)
  pure $ Game i $ fmap Map.fromList a

type Count = Map Colour Int

zipWithMap f = Map.merge Map.dropMissing Map.dropMissing $ Map.zipWithMatched (const f)

one inp = sum $ fmap gameId $ filter (sat . gameTurns) inp
  where
    bound = Map.fromList [(R,12),(G,13),(B,14)]
    sat ms = and $ Map.elems $ zipWithMap (<=) m bound
      where m = Map.unionsWith max ms

two :: [Game] -> Integer
two inp = sum $ fmap (product . Map.elems) $ fmap needed inp
  where
    needed row = Map.unionsWith max $ gameTurns row

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  -- print $ length inp
  -- print $ length $ nub $ toList inp
  print $ one inp
  print $ two inp
