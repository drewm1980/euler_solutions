module Factorization
(divisibleBy
,split
,isPrime
,factorize
) where

-- Check if a number is divisible by another number
divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0

-- Return a pair of numbers that are as close
-- to the square root of a number as possible,
-- as approximate candidates for roots
findSeed :: Int -> (Int,Int,Int)
findSeed k = (floor sqrt, ceiling sqrt, k)
	where sqrt = (fromIntegral k) ** 0.5

-- Break a number into two (probably non-prime) factors
-- If target number is prime, should return (1,target,target)
-- I haven't fully proven this... (1,nonprime,nonprime) may still slip through.
split' :: (Int,Int,Int) -> (Int,Int,Int)
split' (a,b,target) | a*b<target && b<target = split' (a,b+1,target)
		   | a*b>target && a>0 = split' (a-1,b,target)
	           | a*b==target = (a,b,target)
		   | a<0 = error "Something got through!"
		   | b>target = error "Something got through!"

split :: Int -> (Int,Int)
split x = unpack $ split' $ findSeed x
	where unpack = \(a,b,c) -> (a,b)

factorize' :: Int -> [Int] -> [Int]
factorize' n factors | a==1 = n:factors -- Found a prime!
	             | otherwise = (factorize' a []) ++ (factorize' b []) ++ factors
		     where (a,b) = split n

-- Does what it says!
factorize :: Int -> [Int]
factorize n = factorize' n []

-- Test if a number is prime
isPrime :: Int -> Bool
isPrime x = (factorize x) == [x]
