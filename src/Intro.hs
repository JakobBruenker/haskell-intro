-- NOTE: This initial section is just to set up some things to make
-- everything work smoothly, and not part of the actual intro. Skip to
-- the line saying "Haskell Intro" to see the code that's part of the
-- actual presentation.
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Intro where

import Data.List
import Prelude hiding (Maybe(..))
import System.Directory

-- Slightly different name to make things less confusing later on
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

-- Haskell is "lazy": Expressions (or parts of them) are only
-- evaluated once absolutely needed
-- -> that means we can have infinite data structures
-- >>> take 8 [1..]

-- We can also supply functions as arguments to other functions
-- >>> even 34

-- >>> filter even [1..10]

-- Haskell is strongly typed, We haven't seen any types so far!
-- That because they are inferred:
-- >>> :t take

-- `take` takes two arguments: A function and a list, for any type `a`
-- `a` could be e.g. Int, Bool, [Int], or anything else

-- >>> :t take 5

-- We get a type error (at compile time) if they don't match:
-- >>> take [1,2,3]

-- We can manually assert types:
-- >>> 5 :: Int

-- You basically never have to write out types if you don't want to
-- But it's useful when defining functions!

take5 :: [Int] -> [Int]
take5 list = take 5 list

-- >>> take5 [1..10]

-- >>> :t take

-- >>> :t take 5

-- take5 list = take 5 list
-- (take5) list = (take 5) list
-- take5 = take 5

-- x * 2 = (y + 1) * 2
-- x = y + 1

-- List comprehensions are special syntax
-- >>> [ x^2 | x <- [1..10], even x, x > 4 ]

-- we can chain/compose functions with .
-- read from right to left!
process = sort . take 10 . filter even

process' list = sort (take 10 (filter even list))

-- >>> process [100,6,13,600,6,78,500,200,2,6,13,300,42,13,1,8,6,88]

process'' = take 10 . sort . filter even

-- >>> process'' [100,6,13,600,6,78,500,200,2,6,13,300,42,13,1,8,6,88]

-- even returns a Bool
-- >>> :t even

-- >>> even 3

-- The Bool data type is not built in:
data Bool' = True' | False'
-- True' and False' are called "constructors"

-- We can pattern match on these types:
-- ifThenElse :: Bool -> a -> a -> a

-- -> Compiler warns if you have incomplete patterns

-- >>> ifThenElse (6 > 4) "yes" "no"

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Show, Enum)

-- `a` is a type parameter that can stand for any type
-- `Maybe` is a type that can represent failure
data Maybe a = Nothing | Just a
             deriving Show

-- activity :: Weekday -> String

-- >>> [Monday ..]

-- >>> map activity [Monday ..]

-- List syntax is built-in, but the type can be defined manually
data List a = Empty | Node a (List a)
            deriving Show

-- >>> Node 1 (Node 2 (Node 3 Empty))

-- Compare:
-- data [a] = []    | a : [a]

-- >>> 1 : (2 : (3 : []))

-- first :: [a] -> a

-- >>> first [1..5]

-- A non-empty list consists of one element and a (possibly-empty)
-- rest list
data NonEmpty a = a :| [a]

-- first' :: NonEmpty a -> a

-- Encoding invariants in types is a common pattern
-- Making the type system more expressive so you can express
-- more invariants is a recurring theme in Haskell's history
-- -> Aids in type-driven development

-- Let's try to implement take
-- >>> take 5 [1..]

-- No for/while loops - we need to use recursion
-- take' :: Int -> [a] -> [a]

-- >>> take' 5 [1..]

-- prop> \count list -> take count list == take' count list

-- take' (-1) [0] =
-- take' (-1) (0 : []) =
-- 0 : take (-1 - 1) [] =
-- 0 : take (-2) [] =
-- 0 : [] =
-- [0]

-- Let's try to implement filter:
-- >>> filter even [1..10]

-- >>> :t filter 

-- filter' :: (a -> Bool) -> [a] -> [a]

-- where clauses that aren't used in the actual branch won't be evaluated

-- prop> \list -> filter even list == filter' even list

-- >>> [1,2,3] ++ [4,5,6]

-- sort' :: [Int] -> [Int]

-- >>> sort' [5,2,42,1,3,5]

-- prop> \xs -> sort xs == sort' xs

-- Haskell is "purely functional":
-- All examples so far were pure functions
-- - result of function call depends *only* on arguments
-- - No IO (reading files, database access, printing to screen, etc.)
-- - No internal state
-- - No mutable variables
-- We want to separate pure code from impure code as much as possible
-- -> Better Testability, easier to reason about, easy to run in parallel
-- -> If you manage to avoid things like infinite loops, you can be certain
--    that a pure function cannot crash/throw an exception
--    (This is surprisingly attainable in practice)

-- Example:
f :: Int -> Int
f x = x + 2
-- We can conclude that
-- f 4 = 4 + 2 = 6
-- If f were allowed to e.g. print to the console, writing
-- `f 4` and `6` would have different effects

-- -> Useful both for reasoning about code and compiler optimizations

-- Impure functions must be specified as such in their type:
-- Int -> Int -- always pure
-- Int -> IO Int -- can be impure

-- >>> :t listDirectory

-- >>> listDirectory "."

-- example:
-- countFilenamesContaining :: String -> IO Int
-- countFilenamesContaining pattern = do

-- >>> countFilenamesContaining "git"

-- -> General idea: put as much logic as possible into pure functions like
-- `length`, `filter`, `isInfix`, to keep the difficult impure part small
-- Having this enforced by the type system is nice, but it's a useful practice
-- in any language

-- Notable exampels of Haskell being used in industry:
-- Microsoft/Github: compilers and source code analysis
-- Facebook: spam filtering
-- Server backends, financial tech,
-- Host language for DSLs, e.g. hardware design, smart contracts
