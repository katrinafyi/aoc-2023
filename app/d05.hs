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

data Data = Data { seeds :: [Integer], maps :: Map T.Text (T.Text, Map Integer Integer) } deriving (Eq, Show)
-- instance Show Data where
--   show (Data seeds maps) = "Data \n  " ++ show seeds ++ "\n  " ++ show (Map.map fst maps)

-- interval map: 
-- maps *start* of interval to *start* of destination.
-- lookups are performed by rounding down.
intervalLookup x m = dest + x - src
  where
    (src,dest) = fromMaybe (error "interval not in map") $ Map.lookupLE x m

intervalUpdate k (low, len) m
  | src0 == dest0 && src0 <= k = Map.insert k low m `Map.union` Map.singleton (k+len) (k+len)
  | otherwise = error "intervals overlap!"
  where
    (src0,dest0) = fromMaybe (error "interval not in map") $ Map.lookupLE (k + len - 1) m

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
    -- pure $ [low2, low1, len]
  pure (from, (to, foldr' (.) id fns $ Map.singleton 0 0))
  -- pure $ (from, (to, fns))

seedLocation (Data seeds maps) num = fromLeft undefined $ fixM (uncurry go) ("seed", num)
  where
    go "location" num = Left num
    go kind num = Right $ Data.Bifunctor.second (intervalLookup num) $ maps ! kind

-- one :: Data -> Integer
one d = minimum $ fmap (seedLocation d) (seeds d)
  -- mapKeyed (flip intervalLookup $ snd $ maps ! "fertilizer") [0..55]

two d = minimum $ fmap (seedLocation d) seeds'
  where
    seeds' = do
      [low, len] <- chunks 2 $ seeds d
      [low..low+len+1]
    -- goSeed num = rights $ takeWhile isRight $ iterateM (uncurry go) ("seed", num)


main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp
