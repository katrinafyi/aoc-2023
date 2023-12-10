{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

import AocLib
import Control.Applicative
import Control.Arrow hiding (first, second)
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
import qualified Data.Graph.Inductive.NodeMap as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.Graph.Inductive as G
import Control.Exception (assert)

type Pipe = Map Vec2 Vec2
pipe i o = Map.fromList [(i,o),(o,i)]

pars :: Char -> Pipe
pars '|' = pipe north south
pars '-' = pipe east west
pars 'L' = pipe north east
pars 'J' = pipe north west
pars '7' = pipe west south
pars 'F' = pipe east south
pars _ = Map.empty

parse :: [Char] -> (Vec2, Graph Vec2 ())
parse raw = (start, makegraph start grid)
  where
    coords = concat $ indexed2 $ lines raw
    chars = second pars <$> coords
    grid = Map.fromList chars

    Just (start, 'S') = find ((== 'S') . snd) coords

fixstart start g =
  g { gr = G.insEdges (rev <$> G.inn g.gr (g.nod start)) g.gr }
  where rev (a,b,c) = (b,a,c)

makegraph start grid = fixstart start $ runGraph $ do
  let poss = Map.keys grid
  void $ G.insMapNodesM poss
  --  adjs :: Vec2 -> [(Vec2, ())]
  let adjs pos = ((+pos) &&& const ()) <$> Map.elems (grid ! pos)
  forM_ poss $ \p -> do
    let adjpos = fst <$> adjs p
    when (all (`Map.member` grid) adjpos) $
      G.insMapEdgesM $ uncurry (p,,) <$> adjs p

-- PART ONE
one (start, g@Graph{}) = last $ fmap (first g.lab) dists
  where
    dists = G.level (g.nod start) g.gr

-- PART TWO
two (start, g@Graph{}) = assert (all inbound flooded) $ Set.size flooded
  where
    poss = Set.fromList (g.lab <$> G.nodes g.gr)
    inbound = all @[] (`Set.member` poss) . adjacent4

    border = g.lab <$> G.dfs [g.nod start] g.gr
    deltas = zipWith (-) (tail border) border
    rotated = Set.fromList
      $ zipWith (+) (tail border) (fmap rotateRight deltas)

    insidestarts = Set.filter (not . (`elem` border)) rotated

    g' = makeinner g (Set.fromList border)

    flood pos = Set.map g'.lab
      $ Set.fromList 
      $ G.dfs [g'.nod pos] g'.gr

    flooded = Set.unions $ Set.map flood insidestarts

makeinner g@Graph{} border = runGraph $ do
  let poss = Set.fromList (g.lab <$> G.nodes g.gr) Set.\\ border
  void $ G.insMapNodesM $ toList poss

  forM_ poss $ \p -> do
    G.insMapEdgesM $ (p,,()) <$> filter (`Set.member` poss) (adjacent4 p)


main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ snd inp
  print $ one inp
  print $ two inp
