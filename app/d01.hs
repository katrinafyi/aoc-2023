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

data LR = L | R deriving (Eq, Show, Read)

data Move = Move LR Integer deriving (Eq, Show, Read)

rot L = rotateLeft
rot R = rotateRight

repls :: [(T.Text, Int)]
repls =
  [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
    ("zero", 0)
  ]

replAll :: T.Text -> T.Text
replAll d = foldl' go d repls
  where
    go s (word, num) = T.replace word (T.concat [word, T.pack (show num), word]) s

parse raw = fmap T.pack $ lines raw

parsep = readp parser

parser = P.sepBy (integer) (P.char ' ')

one inp =
  sum $
    fmap read $
      map (\x -> [head x, last x]) $
        fmap (filter isDigit) $
          fmap T.unpack $
            inp

two inp = one $ fmap replAll inp

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  -- print $ length inp
  -- print $ length $ nub $ toList inp
  print $ one inp
  print $ two inp
