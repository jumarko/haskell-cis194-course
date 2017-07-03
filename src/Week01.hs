{-# LANGUAGE TemplateHaskell #-}
-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- Choices can also be made based on arbitrary Boolean expressions
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3

-- foo (-3)
-- foo 0
-- foo 1
-- foo 36
-- foo 38

-- Complicated definition of isEven function
-- isEven :: Integer -> Bool
-- isEven n
--   | n `mod` 2 == 0 = True
--   | otherwise      = False

-- Now better?
isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

-- Pairs
--
p :: (Int, Char)
p = (3, 'x')

-- Elements of a pair can be extracted again with pattern matching
sumPair :: (Int,Int) -> Int
sumPair (x,y) = x + y


-- Using functions with multiple arguments
--

f :: Int -> Int -> Int -> Int
f x y z = x + y + z


-- Lists
--

nums, range, range2 :: [Integer]
nums = [1,2,3,19]
range = [1..100]
range2 = [2,4..100]

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame = hello1 == hello2

-- constructing lists
emptyList = []
ex18 = 1 : []
ex19 = 3 : (1 : [])
ex20 = 2 : 3 : 4 : []
ex21 = [2,3,4] == 2 : 3 : 4 : []

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- functions on lists

-- Compute the lenght of a list of Integers
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs


-- Combining functions

-- The number of hailstone steps needed to reach 1 from a starting number
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
