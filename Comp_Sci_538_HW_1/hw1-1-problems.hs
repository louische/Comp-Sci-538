-- CS 839, Spring 2019: Homework 1
-- Part 1: Frequent Words

-- Do not change the following line!
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

-- **Your code should compile without warnings.**
-- The following line makes the compiler treat all warnings as hard errors.
-- When you are done, uncomment it and fix until there are no more errors.
--{-# OPTIONS -Werror #-}
--
-- You might want some functions from these libraries:

import Data.List
import Data.Char
import Data.String

-- This first programming assignment will give you experience writing basic
-- programs in Haskell. You will decompose complex tasks into smaller parts, a
-- common pattern in functional programming. There are two parts to this
-- assignment. Please **do not change** the names and type signatures for the
-- functions below: we will use these names when testing your assignment. Of
-- course, you can introduce new functions if you want.

-- Part 1: Frequent Words (50)
--
--
--
-- You will write a function freqWords that takes a number n and a string and
-- prints the n most frequent words along with how often they appear.  The
-- function should ignore case ("bat" and "BAT" should be treated as the same
-- word) and if there are ties, print out words in alphabetical order.  For
-- instance:
--
-- freqWords 3 "cat bat Foo xray Bat YoYo"
--
-- should return the string
--
-- "bat: 2, cat: 1, foo: 1"
--
-- Note that we count two occurrences of "bat", and we do not print "xray", and
-- "yoyo" since they are after "cat" and "foo" alphabetically.

-- First, define a function that takes a string, and converts it to lower case.
-- (Hint: the toLower and map functions may be helpful.)

makeLower :: String -> String
makeLower = map toLower

-- Next, define a function that splits up a string into a list of strings,
-- breaking at each space.
-- (Hint: the `words` function may be helpful.)

splitSpace :: String -> [String]
splitSpace = words

-- Then, define a function that sorts a list of strings alphabetically.
-- (Hint: the sort function also works on Strings.)

sortAlpha :: [String] -> [String]
sortAlpha = sort

-- Now, we can collect "runs" of identical words into a word and its count. For
-- instance: 
--
-- countRuns ["a", "a", "b", "c", "c", "c"]
--
-- should return
--
-- [("a", 2), ("b", 1), ("c", 3)]
--
-- (Hint: There are several ways to define this function using recursion. The
-- functions `takeWhile`, `dropWhile`, `length`, or `span` may be useful.)
countRuns :: [String] -> [(String, Int)]
countRuns [] = []
countRuns string = (head string, length(takeWhile (== head string) string)) :  countRuns (dropWhile (== head string) string)



-- Next, we can sort the frequency list in decreasing order of frequency.
-- For instance:
--
-- sortFreqs [("a", 2), ("b", 1), ("c", 3)]
--
-- should return
--
-- [("c", 3), ("a", 2), ("b", 1)]
--
-- (Hint: use the `sortBy` function. The `compare` function takes two things of
-- the same type, and returns whether the first argument is lesser than, equal
-- to, or greater than the second argument.)

sortFreqs :: [(String, Int)] -> [(String, Int)]
sortFreqs = sortBy (\ (_, a) (_, b) -> compare (-a) (-b))
-- Now, time for tidying up. Write a function to convert the frequency list into
-- a printable form:
--
-- printFreqs [("c", 3), ("a", 2), ("b", 1)]
--
-- should return
--
-- ["c: 3", "a: 2", "b: 1"]
-- 
-- (Hint: use the `map` function. Use `show` to turn something into a String.)



printFreqs :: [(String, Int)] -> [String]
printFreqs = map (\(a,b) -> a ++ ": " ++ show b)

-- Finally, glue all the pieces together to build the final function. (Hint: to
-- select the correct number of things to print, look at the `take` function.
-- Also look at the `intercalate` function, and use function composition
-- operator (.) to string together functions. Remember that function composition
-- runs backwards: to compose `f :: A-> B` and `g :: B -> C`, write `g . f`.)

freqWords :: Int -> String -> String
freqWords n string = intercalate ", " (take n ((printFreqs . sortFreqs . countRuns . sortAlpha . splitSpace . makeLower) string))

-- A simple main function that asks you for a string of words and how many
-- of the most frequent words you want
main = do
    putStrLn "-- What text would you like to find the frequent words of?"
    putStrLn "-- (Enter all on one line.)"
    words <- getLine
    putStrLn "-- How many of the most frequent words would you like?"
    int_s <- getLine
    let x = read int_s
    print (freqWords x words)

