import AocLib

import Debug.Trace

import qualified Data.Map as Map
import Data.Map(Map, (!))
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq ((:<|)), (|>), (<|), (><))
import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Functor
import Data.Function
import Data.Foldable

import Control.Arrow
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State.Strict

import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP as P

import GHC.Stack (HasCallStack)

data LR = L | R deriving (Eq, Show, Read)
data Move = Move LR Integer deriving (Eq, Show, Read)
rot L = rotateLeft
rot R = rotateRight

parse raw = read ("[" ++ rep ++ "]") :: [Move]
  where rep = raw >>= \x -> if x `elem` "RL" then "Move " ++ [x, ' '] else [x]

parsep = readp parser
parser = P.sepBy (integer) (P.char ' ') 

one inp = manhattan $ snd $ foldl' go ((1,0), (0,0)) inp 
  where 
    go :: (Vec2, Vec2) -> Move -> (Vec2, Vec2)
    go (dir,pos) (Move turn d) = (dir', pos + dir' * fromInteger d)
      where dir' = rot turn dir


two inp = fmap manhattan $ firstDupe $ concatMap thd3 $ scanl' go ((1,0), (0,0), []) inp 
  where 
    go :: (Vec2, Vec2, [Vec2]) -> Move -> (Vec2, Vec2, [Vec2])
    go (dir,pos,seen) (Move turn d) = (dir', pos + dir' * fromInteger d, [pos + dir' * fromInteger i | i <- [1..d]])
      where dir' = rot turn dir

main :: HasCallStack => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  -- print $ length inp 
  -- print $ length $ nub $ toList inp
  print $ one inp
  print $ two inp
