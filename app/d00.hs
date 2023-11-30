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



parse raw = raw

parsep = readp parser
parser = P.sepBy (integer) (P.char ' ') 

one inp = 1

two inp = 2

main :: HasCallStack => IO ()
main = do
  inp <- parsep <$> getContents
  print $ inp
  -- print $ length inp 
  -- print $ length $ nub $ toList inp
  print $ one inp
  print $ two inp
