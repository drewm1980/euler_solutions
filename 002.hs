-- Return entries from the fibonacci sequence
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- The fibonacci sequence as an infinite list
fibs :: [Int]
fibs = 0:1: zipWith (+) (tail fibs) (fibs)

-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
solve :: Int -> Int
solve x = sum $ filter even $ takeWhile (<x) fibs

main :: IO () 
main = do
    let result = solve 4000000
    putStrLn $ "The solution is " ++ show result



