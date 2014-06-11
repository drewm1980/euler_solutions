-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
import Data.List (find)
import Factorization (divisibleBy)

needs_divisible_by = [20,19,18,17,16,15,14,13,12,11]
ub = product needs_divisible_by
lb = product [20,19,17,13,11,7,3]
passes_test n = all (n `divisibleBy`) needs_divisible_by

solution :: Int
solution = case (find passes_test [lb,lb+20..ub]) of 
	Nothing -> -1 
	Just n -> n

fulltest n = all (n `divisibleBy`) [20,19..2]

{-product_of_low_primes = -}

main :: IO () 
main = do
    let result = if (fulltest solution) then solution else -1 
    putStrLn $ "The solution is " ++ show result

solve :: Int -> Int
solve x = x+10

