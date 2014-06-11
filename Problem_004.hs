--Find the largest palindrome made from the product of two 3-digit numbers.

import Data.List

k :: Int
k = 600851475143

-- 999*999 = 998001
--	     997999
--	     997799
--
--	     100001
--
-- 	      10001
-- 100*100 =  10000

palindromize :: Int -> Int
palindromize n
	| n > 999 = error "Out of range"
	| n < 100 = error "Out of range"
	| otherwise = read (strn ++ reverse strn) :: Int
	where strn = show n

-- Because I got stuck without wifi and haskell offline docs are broken
decreasing_list :: Int -> Int -> [Int]
decreasing_list a b = map (a + b -) [b..a]

-- Enumerate candidate palindromes
ub = 997
lb = 100
candidate_palindromes = map palindromize $ decreasing_list ub lb

isThreeDigit :: Int -> Bool
isThreeDigit a = a>99 && a<1000

-- Determine if a number is divisible into two three-digit numbers.
-- Note, if True is returned, result is conclusive.
-- If False is returned, no guarantees!
divisible' :: Int -> Bool
divisible' k | a == 1 = False -- number is prime
	    | otherwise = (isThreeDigit a) && (isThreeDigit b) 
	where (a,b) = split k

main :: IO () 
main = do
    let result = head $ filter divisible' candidate_palindromes
    putStrLn $ "The solution is mayyybe" ++ show result

