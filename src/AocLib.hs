{-# LANGUAGE TypeOperators #-}
module AocLib where

import Data.Maybe
import Data.Either
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Functor
import Data.Function
import Control.Monad
import Control.Arrow
import Control.Applicative
import Text.ParserCombinators.ReadP
import qualified Data.Graph.Inductive.PatriciaTree as G
import Data.String (IsString (..))
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.NodeMap as G

dupe :: b -> (b, b)
dupe x = (x,x)

mapKeyed :: (a -> b) -> [a] -> [(a,b)]
mapKeyed f = fmap (\x -> (x, f x))

-- | Splits the given list into non-overlapping chunks of the given size.
-- The final list might be shorter if the length is not a multiple.
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
  in  ys : chunks n zs

-- | A sliding window of the given size over the list. 
-- All returned lists are the requested size. Input elements might be omitted.
sliding :: Int -> [a] -> [[a]]
sliding _ [] = []
sliding n (x:xs)
  | length front == n = front : sliding n xs
  | otherwise = []
  where front = take n (x:xs)

-- | A sliding window of size 2, returned as a list of pairs.
sliding2 :: [a] -> [(a, a)]
sliding2 (x:y:xs) = (x,y) : sliding2 (y:xs)
sliding2 _ = []

-- | Filters the given list according to the predicate, grouping adjacent 'true' values together.
filterAndGroup :: (a -> Bool) -> [a] -> [[a]]
filterAndGroup f = splitBy (not . f)

lstrip :: String -> String
lstrip = dropWhile isSpace

isEmpty :: String -> Bool
isEmpty = null . lstrip

paragraphs :: [String] -> [[String]]
paragraphs = filterAndGroup (not . isEmpty)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s =
  case dropWhile p s of
    [] -> []
    s' -> w : splitBy p s''
          where (w, s'') = break p s'

split :: Eq a => a -> [a] -> [[a]]
split x = splitBy (== x)

exactly2 :: [a] -> (a,a)
exactly2 [x,y] = (x,y)
exactly2 _ = error "required two elements"

exactly3 :: [a] -> (a,a,a)
exactly3 [x,y,z] = (x,y,z)
exactly3 _ = error "required three elements"

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a
snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b
thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

tweak :: (a -> a) -> [a] -> [a]
tweak _ [] = []
tweak f (x:xs) = f x : xs

indexed :: (Num n, Enum n) => [a] -> [(n,a)]
indexed = zip [0..]

-- | row col
indexed2 :: (Num n, Enum n) => [[a]] -> [[((n,n),a)]]
indexed2 = fmap go . indexed
  where
    go (r,row) = (\(c,x) -> ((r,c),x)) <$> indexed row

minmax :: (Ord a, Foldable t) => t a -> (a,a)
minmax x = (minimum x, maximum x)

-- | 2-vector. Conventionally, +x is to right and +y is down.
instance (Num a, Num b) => Num (a,b) where
  (x,y) + (a,b) = (x+a,y+b)
  (x,y) - (a,b) = (x-a,y-b)
  (x,y) * (a,b) = (x*a,y*b)
  abs (x,y) = (abs x, abs y)
  signum (x,y) = (signum x, signum y)
  fromInteger x = (fromInteger x,fromInteger x)

type Vec2 = (Integer, Integer)
rotateLeft (r,c) = (-c,r)
rotateRight (r,c) = (c,-r)

direction4 :: Num n => [(n, n)]
direction4 = [(0,1),(0,-1),(1,0),(-1,0)]

direction8 :: Num n => [(n, n)]
direction8 = direction4 ++ [(1,1),(1,-1),(-1,1),(-1,-1)]

north, south, east, west :: Num n => (n,n)
north = (-1,0)
south = -north
east = (0,1)
west = -east

adjacent4 pos = (+ pos) <$> direction4
adjacent8 pos = (+ pos) <$> direction8

instance (Num a, Num b, Num c) => Num (a,b,c) where
  (x,y,z) + (a,b,c) = (x+a,y+b,z+c)
  (x,y,z) - (a,b,c) = (x-a,y-b,z-c)
  (x,y,z) * (a,b,c) = (x*a,y*b,z*c)
  abs (x,y,z) = (abs x, abs y, abs z)
  signum (x,y,z) = (signum x, signum y, signum z)
  fromInteger x = (fromInteger x,fromInteger x,fromInteger x)

manhattan :: Num a => (a,a) -> a
manhattan (x,y) = abs x + abs y

uinteger :: ReadP Integer
uinteger = read <$> munch1 isDigit

integer :: ReadP Integer
integer = do
  sign <- option 1 (char '-' $> (-1))
  (* sign) <$> uinteger

uint :: ReadP Int
uint = read <$> munch1 isDigit

int :: ReadP Int
int = do
  sign <- option id (char '-' $> negate)
  sign <$> uint

ints :: String -> [Int]
ints = fmap read . filterAndGroup digit
  where
    digit x = isDigit x || x `elem` "+-"

uints :: String -> [Int]
uints = fmap read . filterAndGroup isDigit

readT :: Read a => T.Text -> a
readT = read . T.unpack

intsT :: Read n => T.Text -> [n]
intsT = fmap readT . filter (not . T.null) . T.split (not . isPMDigit)
  where
    isPMDigit x = isDigit x || x `elem` "+-"

uintsT :: Read n => T.Text -> [n]
uintsT = fmap readT . filter (not . T.null) . T.split (not . isDigit)

line :: ReadP String
line = munch (/= '\n')

line' = line <* char '\n'

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA a b = (Left <$> a) <|> (Right <$> b)

-- iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
-- iterateM n f x = foldM (&) x (replicate n f)

fixM :: Monad m => (a -> m a) -> a -> m a
fixM f = fix (f >=>)

iterateM :: Monad m => (a -> m a) -> a -> [m a]
iterateM f x = go (pure x)
  where 
    go ma = ma : go (ma >>= f)

scanM :: Monad m => (b -> a -> m b) -> b -> [a] -> [m b]
scanM f b0 as = go (pure b0) as
  where 
    go _ [] = []
    go mb (a:as) = mb : go (mb >>= \b -> f b a) as

firstDupe :: Ord a => Foldable t => t a -> Maybe a
firstDupe xs = either Just (const Nothing) $ foldM go Set.empty xs
  where
    go seen x
      | x `Set.member` seen = Left x
      | otherwise = Right (Set.insert x seen)

uniquify :: Ord a => [a] -> [a]
uniquify = Set.toList . Set.fromList

mode :: [Int] -> Int
mode = snd . maximum . map (length &&& head) . group . sort

filterKeys :: (k -> Bool) -> Map.Map k v -> Map.Map k v
filterKeys p = Map.filterWithKey (const . p)

readp :: ReadP a -> String -> a
readp p s = case readP_to_S (p <* skipSpaces <* eof) s of
  [(x,[])] -> x
  [] -> error "readp error: no successful parse"
  x -> error $ "readp error: ambiguous parse"

instance a ~ String => IsString (ReadP a) where
  fromString = string

data Graph a b = Graph { gr :: G.Gr a b, nod :: a -> G.Node, lab :: G.Node -> a }

instance (Show a, Show b) => (Show (Graph a b)) where
  show (Graph gr _ _) = G.prettify gr

runGraph :: (Ord a) => G.NodeMapM a b G.Gr r -> Graph a b
runGraph maker = Graph g node label
  where
    (_,(map,g)) = G.run G.empty maker
    node = fst . G.mkNode_ map
    label = fromJust . G.lab g

mapGraph :: (G.Gr a b -> G.Gr a b) -> Graph a b -> Graph a b
mapGraph f x = x { gr = f (gr x) }
