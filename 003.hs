--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ? 
--
--
{-# LANGUAGE TemplateHaskell #-}

{-import Test.QuickCheck-}
{-import Test.QuickCheck.All-}
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

{--- Take a seed pair, update to try to hit the target value-}
{-updateSeed :: (Int,Int,Int) -> Maybe ((Int,Int,Int),(Int,Int,Int))-}
{-updateSeed (a,b,target)-}
	{-| a == 0 = Nothing-}
	{-| b == target = Nothing-}
	{-| oldproduct == target = Nothing-}
	{-| otherwise = Just ((a,b,target),(newa,newb,target))-}
	{-where-}
	{-oldproduct = a*b-}
	{-newpair = if oldproduct > target then (a-1,b) else (a,b+1)-}
	{-newa = fst newpair-}
	{-newb = snd newpair-}

-- Found these by running
-- unfoldr updateSeed (findSeed k)
seed1 = (653394,919584,600851475143)
seed2 = (630125,953544,600851475143)
seed3 = (492532,1219924,600851475143)
seed4 = (486847,1234168,600851475143)
sol = (486847,1234169,600851475143)

-- Recursion based rather than list based implementation
split :: (Int,Int,Int) -> (Int,Int,Int)
split (a,b,target) | b==target = (-1,-1,-1)
		   | a==target = (-1,-2,-2)
                   | a*b<target = split (a,b+1,target)
		   | a*b>target = split (a-1,b,target)
	           | a*b==target = (a,b,target)

{-prop_updateSeed1 = updateSeed (0,10,11) == Nothing-}
{-prop_updateSeed2 a = updateSeed (a,11,11) == Nothing-}
{-prop_updateSeed3 = updateSeed (2,5,10) == Nothing-}

main :: IO () 
main = do
    {-foo <- test-}
    let result = split (findSeed k)
    putStrLn $ "The solution is mayyybe" ++ show result

{-test :: IO (Bool)-}
{-test = $(quickCheckAll)-}

