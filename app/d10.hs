{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
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
import Data.Map (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Sequence (Seq (..), (<|), (><), (|>))
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

type Pipe = Map Vec2 Vec2
data Data = Data { dataMap :: Map Vec2 Pipe }
  deriving (Eq, Show)

pipe i o = Map.fromList [(i,o),(o,i)]

pars '|' = pipe north south
pars '-' = pipe east west
pars 'L' = pipe north east
pars 'J' = pipe north west
pars '7' = pipe west south
pars 'F' = pipe east south
pars _ = Map.empty


parse raw = (start, startdirs, grid')
  where
    lines' = lines raw
    chars = fmap (second pars) $ concat $ indexed2 lines'
    grid = Map.fromList chars
    Just (start, 'S') = find ((== 'S') . snd)$ concat $ indexed2 lines'

    hasincoming pos dir = maybe False (Map.member (-dir)) $ grid !? (pos + dir)
    startdirs = filter (hasincoming start) direction4
    startpipe = uncurry pipe $ exactly2 startdirs

    grid' = Map.insert start startpipe grid


go _ seen Empty = seen
go grid seen ((pos,dir,n):<|rest)
  | pos `Map.member` seen = seen
  | otherwise = go grid seen' (rest :|> (pos',dir',n+1))
    where
      seen' = Map.insert pos (n,dir) seen
      pos' = pos + dir
      dir' = (grid ! pos') ! (-dir)

zipWithMap f = Map.merge Map.dropMissing Map.dropMissing $ Map.zipWithMatched (const f)

one (start, startdirs, grid) = ("equal at", result, "with distance", d0 ! result)
  where
    d0,d1 :: Map Vec2 Int
    [d0,d1] = map (\s -> Map.map fst $ go grid Map.empty [(start, s, 0)]) startdirs

    equals = filterKeys (/= start) $ Map.filter id $ zipWithMap (==) d0 d1
    result = head $ Map.keys equals

flood grid seen pos
  | pos `Map.member` seen = seen
  | grid ! pos /= Map.empty = Map.insert pos False seen
  | otherwise = traceShow pos' seen'
    where
      floodto x = (Map.empty ==) $ grid ! x
      pos' = filter floodto $ adjacent4 pos

      seen' = foldr (flip $ flood grid) (Map.insert pos True seen) pos'

two (start, startdirs, grid) = (insides, Map.size $ Map.filter id flooded)
  where
    startdir = head startdirs
    border = Map.map snd $ go grid Map.empty [(start, startdir, 0)]

    -- XXX: use rotateRight or rotateLeft that doesn't crash
    insides = (\(pos,d) -> pos + rotateRight d) <$> Map.toList border

    flooded = foldr (flip (flood grid)) Map.empty insides

main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp
