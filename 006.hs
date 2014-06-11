-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

sum_of_squares = sum $ map (^2) [1..100] :: Integer
square_of_sum = (^2) $ sum [1..100] :: Integer
difference = square_of_sum - sum_of_squares

main :: IO () 
main = do
    let result = difference
    putStrLn $ "The solution is " ++ show result

solve :: Int -> Int
solve x = x+10

