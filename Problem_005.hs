-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
--What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

needs_divisible_by = [20,19..1]

{-product_of_low_primes = -}

main :: IO () 
main = do
    let result = solve 100
    putStrLn $ "The solution is " ++ show result

solve :: Int -> Int
solve x = x+10

