-- Week 1 Homework
-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- Validating credit card numbers:
-- Double the value of every second digit beginning from the right: e.g. [1,3,8,6] -> [2,3,16,6]
-- Add the digits of doubles values and the undoubled digits from the original number:
--   [2,3,16,6] -> 2 + 3 + 1 + 6 + 6 = 18
-- Calculate the remainder when the sum is divided by 10

-- Exercise 1: We need to first find the digits of a number
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0 = []
  | n < 10 = [n]
  | otherwise = (mod n 10) : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse(toDigitsRev n)


-- Exercise 2: Double every other digit
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:y:zs) = x : (2*y) : (doubleEveryOtherFromLeft zs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse


-- Exercise 3: define sum of digits
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ys) = (mod x 10) + (div x 10) + (sumDigits ys)


-- Exercise 4: validate function indicates whether an Integer
-- could be a valid credit card number
validate :: Integer -> Bool
validate x = (mod (sumDigits (doubleEveryOther (toDigits x))) 10) == 0



-- Exercise 5: The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
-- Given the number of discs and names for the three pegs
-- returns a list of moves to be performed to move the stack
-- from the first peg to the second
-- Example: hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ (a,b) : (hanoi (n-1) c b a)
