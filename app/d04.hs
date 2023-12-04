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

data Data = Data { gameId :: Int, gameNeed :: [Int], gameHave :: [Int], gameMatches :: Integer } deriving (Eq, Ord, Show)

parse raw = fmap line . T.lines . T.pack $ raw
  where
    line = make . bimap lhs rhs . T.breakOn ": "
    lhs = head . uintsT
    rhs = bimap uintsT uintsT . T.breakOn " | "
    make (n,(need,have)) = Data n need have (genericLength $ need `intersect` have)

one :: [Data] -> Integer
one inp = sum $ fmap go $ fmap gameMatches inp
  where
    go n = if n == 0 then 0 else 2 ^ (n-1)

type S = Map Data Integer

two inp = sum $ foldl' (>>>) id (fmap go (tails inp)) state0
  where
    state0 = Map.fromList $ fmap (,1) $ inp

    go :: [Data] -> S -> S
    go (b:bs) st = Map.unionWith (+) st adjust
      where
        n = st ! b
        adjust = Map.fromList $ map (,n) $ genericTake (gameMatches b) bs
    go [] x = x

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp
