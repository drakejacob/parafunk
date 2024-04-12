import Data.List
import Control.Monad.Par
import Criterion.Main

divConq :: NFData b =>
           (a -> Bool)       -- Granularity control
        -> (a -> (a, a))     -- Split problem
        -> (a -> b)          -- Base case solver
        -> ((b,b) -> b)      -- Merge sub-solutions
        -> a                 -- Input
        -> b                 -- Output
divConq granularity split solve merge input
  = runPar $ go input
  where
    go input
        | granularity input = return $ solve input
        | otherwise = do
                let (a, b) =  split input
                l <- spawn $ go a
                r <- spawn $ go b
                l' <- get l
                r' <- get r
                return $ merge (l', r')

quicksort :: [Int] -> [Int]
quicksort = divConq granularity split solve merge
    where
          granularity a = length a < 10
          split x  = ([i | i <- x, i <= pivot], [i | i <- x, i > pivot])
            where pivot = x !! (length x `div` 2)
          solve         = sort
          merge (a, b)  = a ++ b

-- Sequential quicksort for comparison
seqquicksort :: [Int] -> [Int]
seqquicksort []  = []
seqquicksort [x] = [x]
seqquicksort xs = seqquicksort smaller ++ [pivot] ++ seqquicksort larger
  where
    mid = length xs `div` 2
    pivot = xs !! mid
    (left, right) = splitAt mid xs
    xs' = left ++ tail right
    smaller = [x | x <- xs', x <= pivot]
    larger =  [x | x <- xs', x > pivot]

data Tree = Tree Tree Tree | Leaf Int

sumTree :: Tree  -> Int
sumTree = divConq granularity split solve merge
    where
        granularity (Leaf _)        = True
        granularity (Tree _ _)      = False
        split (Tree ltree rtree)    = (ltree, rtree)
        solve (Leaf n)              = n
        merge (a, b)                = a + b

main:: IO()
main = do
    defaultMain
      [
        bench "sequential quicksort on reverse list" $ 
            nf seqquicksort (reverse [0..9000])
       , bench "quicksort on reverse list" $ 
            nf quicksort (reverse [0..9000])
       , bench "sumTree on large tree" $ 
            nf sumTree largeTree
      ]

largeTree :: Tree
largeTree = Tree
            (Tree
            (Tree
                (Tree
                (Tree
                    (Tree
                    (Tree
                        (Tree
                        (Tree
                            (Tree (Leaf 77) (Leaf 25))
                            (Tree (Leaf 19) (Leaf 63))
                        )
                        (Tree
                            (Tree (Leaf 87) (Leaf 72))
                            (Tree (Leaf 56) (Leaf 21))
                        )
                        )
                        (Tree (Leaf 61)
                        (Tree
                            (Tree (Leaf 82) (Leaf 51))
                            (Leaf 51)
                        )
                        )
                    )
                    (Tree
                        (Tree
                        (Tree (Leaf 5)
                            (Tree (Leaf 84) (Leaf 66))
                        )
                        (Tree (Leaf 3)
                            (Tree (Leaf 38) (Leaf 79))
                        )
                        )
                        (Tree (Leaf 39)
                        (Tree
                            (Tree (Leaf 41) (Leaf 80))
                            (Tree (Leaf 95) (Leaf 17))
                        )
                        )
                    )
                    )
                    (Tree (Leaf 67)
                    (Tree
                        (Tree (Leaf 89)
                        (Tree
                            (Tree (Leaf 57) (Leaf 87))
                            (Leaf 86)
                        )
                        )
                        (Tree (Leaf 43)
                        (Tree (Leaf 81) (Leaf 46))
                        )
                    )
                    )
                )
                (Tree (Leaf 81) (Leaf 42))
                )
                (Leaf 78)
            )
            (Tree
                (Tree
                (Tree (Leaf 54) (Leaf 22))
                (Tree
                    (Tree (Leaf 27)
                    (Tree
                        (Tree (Leaf 56) (Leaf 7))
                        (Tree
                        (Tree (Tree (Leaf 68) (Leaf 69)) (Leaf 60))
                        (Leaf 6)
                        )
                    )
                    )
                    (Tree (Leaf 21)
                    (Tree
                        (Tree
                        (Tree (Tree (Leaf 28) (Leaf 76))
                            (Tree (Leaf 76) (Leaf 67))
                        )
                        (Tree
                            (Tree (Leaf 6) (Leaf 83))
                            (Tree (Leaf 31) (Leaf 11))
                        )
                        )
                        (Leaf 28)
                    )
                    )
                )
                )
                (Leaf 60)
            )
            )
            (Tree
            (Tree
                (Tree
                    (Tree
                        (Tree (Leaf 75)
                        (Tree
                            (Tree
                            (Tree (Tree (Leaf 4) (Leaf 8))
                                (Tree (Leaf 77) (Leaf 68))
                            )
                            (Leaf 100)
                            )
                            (Tree (Leaf 72)
                            (Tree
                                (Tree (Leaf 33) (Leaf 14))
                                (Tree (Leaf 29) (Leaf 55))
                            )
                            )
                        )
                        )
                        (Tree
                        (Tree
                            (Tree (Tree (Leaf 1)
                            (Tree (Leaf 44) (Leaf 100))
                            )
                            (Leaf 38)
                        )
                        (Tree
                            (Tree (Tree (Leaf 77) (Leaf 57)) (Leaf 22))
                            (Leaf 66)
                        )
                        )
                        (Leaf 12)
                    )
                    )
                (Tree
                (Tree
                    (Tree
                    (Tree (Tree (Leaf 91)
                        (Tree (Leaf 41) (Leaf 92))
                    )
                    (Leaf 35)
                    )
                    (Tree
                    (Tree (Tree (Leaf 76) (Leaf 74)) (Leaf 70))
                    (Tree (Tree (Leaf 1) (Leaf 83)) (Leaf 89))
                    )
                )
                (Tree
                    (Tree
                    (Tree (Tree (Leaf 38) (Leaf 55)) (Leaf 44))
                    (Tree
                        (Tree (Leaf 7) (Leaf 81))
                        (Tree (Leaf 64) (Leaf 37))
                    )
                    )
                    (Tree (Leaf 1)
                    (Tree
                        (Tree (Leaf 51) (Leaf 71))
                        (Tree (Leaf 70) (Leaf 51))
                    )
                    )
                )
                )
                (Tree
                (Tree
                    (Tree
                    (Tree (Leaf 14)
                        (Tree (Leaf 65) (Leaf 21))
                    )
                    (Leaf 87)
                    )
                    (Tree
                    (Tree (Tree (Leaf 85) (Leaf 50)) (Leaf 70))
                    (Tree (Leaf 14) (Tree (Leaf 88) (Leaf 47)))
                    )
                )
                (Tree
                    (Tree
                    (Tree (Tree (Leaf 32) (Leaf 78)) (Leaf 47))
                    (Leaf 89)
                    )
                    (Leaf 50)
                )
                )
            )
            )
            (Leaf 84)
        )
        (Leaf 70)
        )