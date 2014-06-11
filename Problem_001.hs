
main :: IO () 
main = do
    let result = solve 999
    putStrLn $ "The solution is " ++ show result

-- Find the sum of all the multiples of 3 or 5 below 1000.

filt :: Int -> Bool
filt x = (mod x 3 == 0) || (mod x 5 == 0)

solve :: Int -> Int
solve x = sum $ filter filt [1..x]

