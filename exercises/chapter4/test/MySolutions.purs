module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (length, head, tail, null, filter, concatMap, (..), (:))
import Data.Foldable (product, foldl, foldr)
import Data.Maybe (fromMaybe)

-- Return true if input is an even integer
isEven :: Int -> Boolean
isEven n =
  if n < 0
    then isEven (-n)
    else if n == 0
      then true
      else not (isEven (n - 1))


oneIfEven :: Int -> Int 
oneIfEven x = if isEven x then 1 else 0

-- Recursive function to count number of even integers in array
countEven :: Array Int -> Int 
countEven arr = 
  if null arr then 
    0 
  else
    oneIfEven (fromMaybe 1 $ head arr) + countEven (fromMaybe [] (tail arr))
  
-- Map exercises 
squared :: Array Number -> Array Number
squared xs = (\n -> n * n) <$> xs

keepNonNegative :: Array Number -> Array Number
keepNonNegative xs = filter (\x -> x >= 0.0) xs

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number 
keepNonNegativeRewrite xs = (\x -> x >= 0.0) <$?> xs

-- Flattening Arrays
pairs' :: Int -> Array (Array Int)
pairs' n = 
  concatMap (\i -> 
    map (\j -> [i, j]) (1 .. n)
  ) (1 .. n)

pairs'' :: Int -> Array (Array Int)
pairs'' n =
  concatMap (\i -> 
    map (\j -> [i, j]) (i .. n)
    ) (1 .. n)

factors' :: Int -> Array (Array Int)
factors' n = filter (\pair -> product pair == n) (pairs'' n)

-- Using do notation
factors'' :: Int -> Array (Array Int)
factors'' n = filter (\xs -> product xs == n) do 
  i <- 1 .. n 
  j <- i .. n
  pure [ i, j ]

-- Guards
factorsV3' :: Int -> Array (Array Int)
factorsV3' n = do
  i <- 1 .. n 
  j <- i .. n 
  guard $ i * j == n
  pure [ i, j ]

-- ---------------------------------------------------------------------- 
-- Exercises
-- ---------------------------------------------------------------------- 

isPrime :: Int -> Boolean
isPrime n = n /= 1 && length (factorsV3' n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do 
  x <- xs 
  y <- ys 
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do 
  x <- 1 .. n
  y <- 1 .. n  
  z <- 1 .. n
  guard $ x*x + y*y == z*z &&
          x < y &&
          x < z &&
          y < z
  pure [ x, y, z ]

primeFactors :: Int -> Array Int
primeFactors n = factorize 2 n
  where
  factorize :: Int -> Int -> Array Int
  factorize _ 1 = []
  factorize divisor dividend =
    if dividend `mod` divisor == 0 then
      divisor : factorize (divisor) (dividend / divisor)
    else
      factorize (divisor + 1) dividend

-- ---------------------------------------------------------------------- 
-- Tail Recursion
-- ---------------------------------------------------------------------- 

-- Checking tail recursion
-- Note: free and tailrec packages 
f :: Int -> Int
f n = 
  if n == 0 
    then 0
    else 1 + f (n - 1)

-- A call is in tail position when it is the last call made before a function 
-- returns. This is the reason why we observed a stack overflow in the example 
-- - the recursive call to f was not in tail position.

factorialTailRec' :: Int -> Int -> Int
factorialTailRec' n acc = 
    if n == 0 
      then acc 
      else factorialTailRec' (n - 1) (acc * n)

-- ---------------------------------------------------------------------- 
-- Accumulators
-- ---------------------------------------------------------------------- 

-- Make recursive functions tail recursive, by introducing a second function 
-- argument to accumulate the result instead: 

lengthTailRec' :: forall a. Array a -> Int 
lengthTailRec' arr = length' arr 0 
  where
    length' :: Array a -> Int -> Int
    length' arr' acc = 
      if null arr' 
        then acc 
        else length' (fromMaybe [] $ tail arr') (acc + 1)

-- Prefer folds to explicit recursion
reverseFoldr :: forall a. Array a -> Array a 
reverseFoldr = foldr (\x xs -> xs <> [x]) []

reverse :: forall a. Array a -> Array a 
reverse = foldl (\xs x -> x : xs) []

-- ---------------------------------------------------------------------- 
-- Exercises
-- ---------------------------------------------------------------------- 

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\acc b -> if not acc then acc else b == true) true

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fib' n 2 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) n2 (n1 + n2)

