-- NOTE: This initial section is just to set up some things to make
-- everything work smoothly, and not part of the actual intro. Skip to
-- the line saying "Haskell Intro" to see the code that's part of the
-- actual presentation.
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NegativeLiterals #-}

module Intro where

import Data.List
import Prelude hiding (Maybe(..))
import System.Directory

isInfix :: String -> String -> Bool
isInfix = isInfixOf

-- Haskell Intro
-- =============

-- Getting lost? Feel free to interrupt.

-- Haskell is a strongly typed, purely functional language
-- Exists since ~1990
-- Other languages are taking inspiration:
-- e.g. Java introduced lambdas & streams, is now introducing
-- algebraic data types and pattern matching

-- You can evaluate ordinary mathematical expressions
-- >>> 24 * 10 + 5

-- Functions are applied with spaces instead of parentheses:
-- Instead of sqrt(25), we write
-- >>> sqrt 25

-- Lists can be written with square brackets
-- >>> sort [4,2,5,1,6,22,5]

-- XXX Perhaps a better function to introduce these next few
-- XXX steps would be take, it's not generic and not a higher-
-- XXX order function

-- We can supply functions as arguments to other functions
-- >>> even 34

-- >>> filter even [1..10]

-- We haven't seen any types so far! That because they are inferred:
-- >>> :t even

-- >>> :t filter
-- filter :: (a -> Bool) -> [a] -> [a]

-- `filter` takes two arguments: A function and a list

-- We get a type error (at compile time) if they don't match:
-- >>> filter [1,2,3]

-- We can also ask about the types of compound expressions:
-- >>> :t filter even

filterEven :: [Int] -> [Int]
filterEven list = filter even list

-- >>> filterEven [1..10]

-- >>> :t filter
-- filter :: (a -> Bool) -> [a] -> [a]

-- >>> :t filter even
-- filter even :: Integral a => [a] -> [a]

-- filterEven list = filter even list
-- (filterEven) list = (filter even) list
-- filterEven = filter even

-- x * 2 = (y + 1) * 2
-- x = y + 1

process = sort . take 10 . filter even

process' list = sort (take 10 (filter even list))

-- By default, every expression is lazy: It will only get evaluated
-- as far as is absolutely necessary
-- >>> process [1..]
-- [2,4,6,8,10,12,14,16,18,20]

-- >>> :t even

-- The Bool data type is not built in:
data Bool' = True' | False'

-- We can pattern match on these types:
-- ifThenElse :: Bool -> a -> a -> a
-- ifThenElse condition a b = _

-- >>> ifThenElse (6 > 4) "yes" "no"

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

-- `a` is a type parameter that can stand for any type
-- `Maybe` is a type that can represent failure
data Maybe a = Nothing | Just a

-- activity :: Weekday -> Maybe String

-- List syntax is built-in, but the type can be defined
data List a = Empty | Cons a (List a)

-- Compare:
-- data [a] = []    | a : [a]

-- >>> [1,2,3] == 1 : (2 : (3 : []))

-- first :: [a] -> a

take' :: Int -> [Int] -> [Int]
take' count [] = []
take' 0 (a : as) = []
take' count (a : as) = a : take (count - 1) as
-- take' count (a : as) | count <= 0 = []
--                      | otherwise  = a : take (count - 1) as

-- XXX Maybe go through this derivation
-- >>> take' -1 [0]
-- [1]
-- take' -1 (0 : []) = 0 : take (-1 - 1) []
-- take' -1 (0 : []) = 0 : take -2 []
-- take' -1 (0 : []) = 0 : [] = [0]

-- prop> \count list -> take count list == take' count list
-- *** Failed! Falsified (after 11 tests and 5 shrinks):
-- -1
-- [0]

-- Let's try to implement filter:
-- >>> :t filter 

-- XXX Maybe use the ifThenElse function from earlier here
-- XXX or guards
-- XXX Also really think about how to explain this I suppose
-- There are no while or for-loops in Haskell, instead we always
-- use recursion
-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' predicate [] = []
-- filter' predicate (a : as) = ifThenElse (predicate a) (a : rest) rest
--   where rest = filter' predicate as

-- prop> \list -> filter even list == filter' even list

-- >>> [1,2,3] ++ [4,5,6]

-- quickSort :: [Int] -> [Int]
-- quickSort [] = []
-- quickSort (x : rest) =
--   quickSort (filter (<= x) rest) ++
--   [x] ++
--   quickSort (filter (> x) rest)
  
-- >>> quickSort [1,5,2,42,3,5]

-- prop> \xs -> sort xs == quickSort xs

-- Haskell is purely functional:
-- All examples so far were pure functions
-- - result of function call depends *only* on arguments
-- - No IO (reading files, database access, printing to screen, etc.)
-- - No internal state
-- - No mutable variables
-- We want to separate pure code from impure code as much as possible
-- -> Better Testability, easier to reason about, easy to run in parallel

-- Impure functions must be specified as such in their type:
-- Int -> Int -- always pure
-- Int -> IO Int -- can be impure

-- >>> :t listDirectory

-- >>> listDirectory "."
-- ["src","README.md","notes.txt","LICENSE","hie.yaml","haskell-intro.cabal","CHANGELOG.md",".vscode",".gitignore",".git"]

-- example:
-- XXX Maybe not the best example
countFilenamesContaining :: String -> IO Int
countFilenamesContaining pattern = do
  fileNames <- listDirectory "."
  let count = length (filter (isInfix pattern) fileNames)
  pure count
  
-- XXX Just thinking about how to present this:
-- countFilenamesContaining' :: String -> IO Int
-- countFilenamesContaining' pattern = do
--   let fileNames = listDirectory "." -- second line
--   let count = length fileNames
--   -- third line ^^ introduce length first, then fix
--   pure 0 -- start with this line
  
-- >>> countFilenamesContaining "git"
