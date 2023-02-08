module Test.MyExamples where

import Prelude
import Control.Alternative (guard)
import Data.Array (concatMap, filter, null, tail, (..), (:))
import Data.Foldable (product)
import Data.Maybe (fromMaybe)
import Data.Path (Path, ls)

myfactorial :: Int -> Int 
myfactorial n = 
  if n == 0 then 
    1
  else 
    n * myfactorial (n-1)

myfib :: Int -> Int 
myfib n =
  if n == 0 then 
    0
  else if n == 1 then 
    1 
  else 
    myfib (n - 1) + myfib (n - 2)

-- Compute length of array using recursion
length :: forall a. Array a -> Int
length arr = 
  if null arr then 
    0
  else 
    1 + (length $ fromMaybe [] $ tail arr)

