--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ? 

import Data.List

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0

k :: Int
k = 600851475143

-- Return a pair of numbers that are as close
-- to the square root of a number as possible,
-- as approximate candidates for roots
findSeed :: Int -> (Int,Int,Int)
findSeed k = (floor sqrt, ceiling sqrt, k)
	where sqrt = (fromIntegral k) ** 0.5

-- Break a number into two (probably non-prime) factors
split :: (Int,Int,Int) -> (Int,Int,Int)
split (a,b,target) | b==target = (-1,-1,-1)
		   | a==target = (-1,-2,-2)
                   | a*b<target = split (a,b+1,target)
		   | a*b>target = split (a-1,b,target)
	           | a*b==target = (a,b,target)

main :: IO () 
main = do
    let result = split (findSeed k)
    putStrLn $ "The solution is mayyybe" ++ show result

