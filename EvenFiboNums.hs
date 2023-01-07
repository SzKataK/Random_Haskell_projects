module EvenFiboNums where

-- https://projecteuler.net/problem=2

fibo :: [Integer]
fibo = map fst (iterate (\(x,y) -> (y, x + y)) (1, 2))

sumEvenFibo :: Integer
sumEvenFibo = sum (filter even (takeWhile (<4000000) fibo))

