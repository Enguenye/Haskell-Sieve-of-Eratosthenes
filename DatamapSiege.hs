import Data.Map (Map)
import qualified Data.Map as Map

main = putStrLn $ show (take 200 primes)
sieve xs = sieve' xs Map.empty
    where
        sieve' [] table = []
        sieve' (x:xs) table =
         case Map.lookup x table of
          Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
          Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
            where
                reinsert table prime = Map.insertWith (++) (x+prime) [prime] table
wheel235 = 4:2:4:2:4:6:2:6:wheel235
spin (x:xs) n = n : spin xs (n + x)
primes = 2 : 3 : 5 : sieve (spin wheel235 7)
