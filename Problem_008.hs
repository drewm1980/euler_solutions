-- There is no problem zero; this is just a template

main :: IO () 
main = do
    let result = solve 100
    putStrLn $ "The solution is " ++ show result

solve :: Int -> Int
solve x = x+10

