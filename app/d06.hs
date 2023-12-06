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


parse raw = (go raw, go $ filter (/= ' ') raw)
  where
    go raw = fmap exactly2 $ transpose $ fmap (fmap fromIntegral) $ fmap ints $ lines raw

-- d = (tmax - thold) * thold - dneed
-- t = maximum time
-- x = time held
-- d = distance needed

solve (t,d) = 1 + floor root2 - ceiling root1
  where
    f x = (t - x) * x - d
    check delta x = if ceiling x == floor x then x + delta else x
    root1 = check 1 $ 0.5 * (t - sqrt (t*t - 4 * d))
    root2 = check (-1) $ 0.5 * (t + sqrt (t*t - 4 * d))

one (inp,_) = product $ fmap solve inp
two = one . swap

main :: (HasCallStack) => IO ()
main = do
  inp <- parse <$> getContents
  print $ inp
  print $ one inp
  print $ two inp
