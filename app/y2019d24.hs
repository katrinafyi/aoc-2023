{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import AocLib
import AocLib (adjacent4)
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State.Strict hiding (state)
import Data.Bool
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
import Data.Tuple
import Debug.Trace
import GHC.Stack (HasCallStack)
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P

bound = 4
range = [0..bound]

indices :: Set Vec2
indices = Set.fromList [(r,c) | r <- range, c <- range]

type Data = Map (Int, Vec2) Int

parse :: String -> Data
parse raw = Map.mapKeys (0,) $ Map.map (bool 0 1 . (== '#')) $ Map.fromList inp
  where
    inp = concat $ indexed2 (filter (not . isSpace) <$> lines raw)


step grid = Map.mapWithKey go grid
  where
    get pos = fromMaybe 0 (grid Map.!? (0,pos))
    adjs pos = sum $ get <$> adjacent4 pos

    go (_,pos) (0) = if a == 1 || a == 2 then 1 else 0
      where a = adjs pos
    go (_,pos) (1) = if a == 1 then 1 else 0
      where a = adjs pos
    go _ _ = undefined

one inp = (sum calc)
  where
    final = fromJust $ firstDupe $ iterate step inp
    calc = zipWith (*) (fmap (2 ^) [0 ..]) $ fmap snd $ Map.toList final


-- | extra adjacencies due to recursion (upwards or downwards)
adj' (d,(1,2)) = (d+1,) <$> [(0,i) | i <- range]
adj' (d,(2,1)) = (d+1,) <$> [(i,0) | i <- range]
adj' (d,(2,3)) = (d+1,) <$> [(i,bound) | i <- range]
adj' (d,(3,2)) = (d+1,) <$> [(bound,i) | i <- range]
adj' (d,(r,c)) = 
  when (r == 0) (d-1,(1,2))
  ++ when (c == 0) (d-1,(2,1))
  ++ when (c == bound) (d-1,(2,3))
  ++ when (r == bound) (d-1,(3,2))
  where 
    when True x = [x]
    when False _ = []

step' grid = Map.mapWithKey go (grid `Map.union` inner `Map.union` outer)
  where
    dmax = fst $ fst $ Map.findMax grid
    dmin = fst $ fst $ Map.findMin grid
    inner = Map.fromSet (const 0) $ Set.map (dmin-1,) indices
    outer = Map.fromSet (const 0) $ Set.map (dmax+1,) indices

    get pos = fromMaybe 0 (grid Map.!? pos)
    adjs pos@(d,p) = sum $ get <$> adj' pos ++ fmap (d,) (adjacent4 p)

    go (_,(2,2)) 0 = 0
    go (pos) (0) = if a == 1 || a == 2 then 1 else 0
      where a = adjs pos
    go (pos) (1) = if a == 1 then 1 else 0
      where a = adjs pos
    go pos v = error $ "go: " ++ show pos ++ " " ++ show v

two inp = calc
  where 
    final = iterate step' inp !! 200
    calc = Map.size $ Map.filter (== 1) final

showGrid grid = for_ [dmin..dmax] layer
  where
    dmax = fst $ fst $ Map.findMax grid
    dmin = fst $ fst $ Map.findMin grid

    layer d = do
      putChar '\n'
      putStrLn $ "depth " ++ show d
      for_ [0..bound] $ \r -> do
        for_ [0..bound] $ \c -> 
          let x = grid Map.! (d,(r,c)) in
            putChar $ if x == 0 then '.' else '#'
        putChar '\n'


main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  -- print $ length inp
  -- print $ length $ nub $ toList inp
  print $ one inp
  -- showGrid $ two inp
  print $ two inp
  -- showGrid inp
