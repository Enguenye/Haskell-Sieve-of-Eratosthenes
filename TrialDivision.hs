main = putStrLn $ show primes
primes = 2 : [x | x <- [3..100], isprime x]
isprime x = all (\p -> x `mod` p > 0) (factorsToTry x)
    where
        factorsToTry x = takeWhile (\p -> p*p <= x) primes
