-- CS 839, Spring 2019: Homework 1
-- Part 2: Numbers to Words

-- Do not change the following line!
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

-- **Your code should compile without warnings.**
-- The following line makes the compiler treat all warnings as hard errors.
-- When you are done, uncomment it and fix until there are no more errors.
{-# OPTIONS -Werror #-}
--
-- You might want some functions from these libraries:

import Data.List
import Data.Char

-- This first programming assignment will give you experience writing basic
-- programs in Haskell. You will decompose complex tasks into smaller parts, a
-- common pattern in functional programming. There are two parts to this
-- assignment. Please **do not change** the names and type signatures for the
-- functions below: we will use these names when testing your assignment. Of
-- course, you can introduce new functions if you want.

-- Part 2: Numbers to Words (50)
--
--
--
-- You will write a function to translate numbers to English words. We will
-- assume that the number is at most 6 digits, and is non-negative. (Your
-- function can do anything if the input is not of this form.) The final
-- function:
--
-- printNum 123456
--
-- should return
--
-- "one hundred twenty-three thousand four hundred fifty-six"
--
-- (Note the dashes.) As before we will build up to this function by first
-- building printing functions for one digit, two digit, and three digit
-- numbers.
--
-- We will use three lists of number names:
ones = [ "zero", "one", "two", "three", "four"
       , "five", "six", "seven", "eight", "nine"]
teens = [ "ten", "eleven", "twelve", "thirteen", "fourteen"
        , "fifteen" , "sixteen", "seventeen", "eighteen", "nineteen" ]
tens = [ "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety" ]

-- The indexing operation `!!` will be useful to get elements. For instance:
--
-- ones !! 0
--
-- returns "zero", while
--
-- tens !! 3
--
-- returns "fifty". Note that the first element is at index 0.

-- Each function that prints numbers takes an Int and returns a string.
-- This function is called by main. Redefine it to printNum2, printNum3, and
-- printNum6 as you define them.
printNum :: Int -> String
printNum = printNum6

-- Let's warm up by writing a function for single-digit numbers. This function
-- should work for all single-digit numbers: 0, 1, ..., 9
printNum1 :: Int -> String
printNum1 n = ones!!n 

-- Write a similar function to handle numbers with at most 2 digits. It should
-- work on numbers 0, 1, ..., 99. For instance:
--
-- printNum2 0  == "zero"
-- printNum2 10 == "ten"
-- printNum2 15 == "fifteen"
-- printNum2 34 == "thirty-four"
--
-- (Hint: there are three cases---single digit, two digit with first digit 1,
-- and two digit with first digit 2. Try to reuse printNum1 for the first case.
-- Make sure you add the dash for the last case but note that printNum2 30
-- should return "thirty", not "thirty-zero". The functions `div` and `mod` may
-- come in handy.)

printNum2 :: Int -> String
printNum2 n
    | n < 10 && n > 0 = ones!!n
    | n >= 10 && n < 20 = teens!!(n - 10)
    | n >= 20 && (mod n 10 == 0) = tens!!(div n 10 - 2)
    | otherwise  = tens!!(div n 10 - 2) ++ "-" ++ printNum1 (mod n 10)

-- Write a similar function to handle numbers with at most 3 digits. It should
-- work on numbers 0, 1, ..., 999. Try to reuse printNum2. For instance:
--
-- printNum3 15  == "fifteen"
-- printNum3 101 == "one hundred one"
--
-- Note that we don't put an "and" in "one hundred one".

printNum3 :: Int -> String
printNum3 n
    | n < 100 = printNum2 n
    | n `mod` 100 == 0 = ones!!(n `div` 100) ++ " hundred" 
    | otherwise = ones!!(n `div` 100) ++ " hundred " ++ printNum2 (n `mod` 100)

-- Finally, write a function print numbers up to six digits: 0, 1, ..., 999999.
-- Make sure to reuse printNum3. For instance:
--
-- printNum 123000 == "one hundred twenty-three thousand"
-- printNum 950001 == "nine hundred fifty thousand one"
--
-- We don't put an "and", but it is not hard to add this extension.

printNum6 :: Int -> String
printNum6 n
    | n < 1000 = printNum3 n
    | n `mod` 1000 == 0 = printNum3 (n `div` 1000) ++ " thousand"
    | otherwise = printNum3 (n `div` 1000) ++ " thousand " ++ printNum3 (n `mod` 1000)

-- An infinite loop that keeps asking you for a number to convert to English
-- words. It calls the printNum function, which is defined on line 64: you may
-- wish to first assign it to printNum1, and then assign it to printNum2,
-- printNum3, and printNum6 as you complete those functions. 
-- It loops infinitely, asking for input. You can terminate it with CTRL-C or
-- CTRL-D.
main = do
    putStrLn "-- Please enter a number."
    int_s <- getLine
    let x = read int_s
    print (printNum x)
    main
