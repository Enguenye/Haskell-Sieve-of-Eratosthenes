main = putStrLn $ show primes
primes = sieve [2..100]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
sieve []=[]
