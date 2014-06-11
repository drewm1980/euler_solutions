--The prime factors of 13195 are 5, 7, 13 and 29.
--What is the largest prime factor of the number 600851475143 ? 

import Data.List
import Factorization

k = 600851475143 :: Int

main :: IO () 
main = do
    let result = maximum $ factorize k
    putStrLn $ "The solution is " ++ show result

