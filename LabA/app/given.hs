import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq 
import Control.Monad.Par (parMapM, runPar, spawn, get)

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int

mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1

resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))

-- sequential jackknife
jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500

-- parallel jackknife
jackknifePar :: NFData b => (([a] -> b) -> [[a]] -> [b]) -> ([a] -> b) -> [a] -> [b]
jackknifePar mapFunc f = mapFunc f . resamples 500

-- define parallel map function
pmap :: NFData b => (a -> b) -> [a] -> [b] 
pmap f [] = []
pmap f (x:xs) = par current $ pseq rest (current:rest)
  where current = force $ f x
        rest = pmap f xs

-- map function using rpar and rseq
rmap :: NFData b => (a -> b) -> [a] -> [b]
rmap f [] = [] 
rmap f (x:xs) = runEval $ do
      y  <- rpar (force $ f x)
      ys <- rseq (rmap f xs)
      -- let ys = rmap f xs
      -- rseq y
      return (y:ys)

-- map using Strategies
smap :: NFData b => (a -> b) -> [a] -> [b]
smap f xs = map f xs `using` parList rdeepseq

-- Par monad map function
parMapPar :: NFData b => (a -> b) -> [a] -> [b]
parMapPar f xs = runPar $ do
    results <- mapM (spawn . return . f) xs
    mapM get results

--------------------------------------------------------------------
crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)

  defaultMain
        [
        bench "sequential jackknife"          (nf (jackknife mean) rs)
        , bench "standard par jackknife"      (nf (jackknifePar pmap mean) rs)
        , bench "jackknife using Eval monad"  (nf (jackknifePar rmap mean) rs)
        , bench "jackknife using parMap"      (nf (jackknifePar (parMap rdeepseq) mean) rs)
        , bench "jackknife using Strategies"  (nf (jackknifePar smap mean) rs)
        , bench "jackknife using Par monad"   (nf (jackknifePar parMapPar mean) rs)
         ]