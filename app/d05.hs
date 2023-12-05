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

data Data = Data { seeds :: [Integer], maps :: Map T.Text (T.Text, Map Integer Integer) } deriving (Eq, Show)
-- instance Show Data where
--   show (Data seeds maps) = "Data \n  " ++ show seeds ++ "\n  " ++ show (Map.map fst maps)

-- interval map: 
-- maps *start* of interval to *start* of destination.
-- lookups are performed by rounding down.
intervalLookup x m = dest + x - src
  where
    (src,dest) = fromMaybe (0,0) $ Map.lookupLE x m

intervalUpdate k (low, len) m
  | src0 == dest0 && src0 <= k = Map.insert k low m `Map.union` Map.singleton (k+len) (k+len)
  | otherwise = error "intervals overlap!"
  where
    (src0,dest0) = fromMaybe (error "interval not in map") $ Map.lookupLE (k + len - 1) m

intervalImage m (k,len) = front ++ intervals
  where
    v = intervalLookup k m
    between = Map.insert k v $ fst $ Map.split (k+len) $ snd $ Map.split k m
    intervals = fmap (\((k1,v1),(k2,_)) -> (v1, k2-k1)) $ sliding2 $ Map.toList between
    len' = sum $ fmap snd intervals
    front = guard (len' < len) $> (snd (Map.findMax between), len - len')

parse raw =
  -- Map.fromList maps
  Data (fmap fromIntegral $ uints $ head ls) (Map.fromList maps)
  where
    ls = lines raw
    maps = readp (P.sepBy parsep $ P.string "\n\n") $ unlines $ drop 2 ls

parsep = do
  h <- T.pack <$> line'
  let [from,to] = T.splitOn "-to-" $ fst $ T.breakOn " " h
  fns <- flip P.sepBy (P.char '\n') $ do
    [low2, low1, len] <- uinteger `P.sepBy` P.char ' '
    pure $ intervalUpdate low1 (low2, len)
  pure (from, (to, foldr' ($) (Map.singleton 0 0) fns))

seedLocation (Data seeds maps) interval = fromLeft undefined $ fixM (uncurry go) ("seed", [interval])
  where
    go "location" is = Left is
    go kind is = Right (fst m, concatMap (intervalImage (snd m)) is)
      where m = maps ! kind

one d = minimum $ concatMap (seedLocation d . interval) (seeds d)
  where interval x = (x,1)

two d = minimum $ concatMap (seedLocation d) seeds'
  where
    seeds' = fmap (\[x,y] -> (x,y)) $ chunks 2 $ seeds d

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  -- print $ intervalImage (snd $ maps inp ! "seed") (98, 4)
  -- print $ flip intervalLookup (snd $ maps inp ! "seed") <$> [98 .. (98 + 3)]
  print $ one inp
  print $ two inp
