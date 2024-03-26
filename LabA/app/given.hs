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

jackknife0 :: ([a] -> b) -> [a] -> [b]
jackknife0 f = map f . resamples 500

jackknife1 :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife1 f = pmap f . resamples 500

-- define parallel map function
pmap :: NFData b => (a -> b) -> [a] -> [b] 
pmap f [] = []
pmap f (x:xs) = par current $ pseq rest (current:rest)
  where current = force $ f x
        rest = pmap f xs

jackknife2 :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife2 f = rmap f . resamples 500

jackknife2b :: ([a] -> b) -> [a] -> [b]
jackknife2b f = parMap rseq f . resamples 500

-- define parallel map function using rpar and rseq
rmap :: NFData b => (a -> b) -> [a] -> [b]
rmap f [] = [] 
rmap f (x:xs) = runEval $ do
      y  <- rpar (force $ f x)
      ys <- rseq (rmap f xs)
      -- let ys = rmap f xs
      -- rseq y
      return (y:ys)

jackknife3 :: NFData b =>  ([a] -> b) -> [a] -> [b]
jackknife3 f = smap f. resamples 500

smap :: NFData b => (a -> b) -> [a] -> [b]
smap f xs = map f xs `using` parList rdeepseq 

-- using Par monad
jackknife4 :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife4 f = parMapPar f . resamples 500 

parMapPar :: NFData b => (a -> b) -> [a] -> [b]
parMapPar f xs = runPar $ do
    ibs <- mapM (spawn . return . f) xs
    mapM get ibs


--------------------------------------------------------------------
crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife0 mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain
        [
        bench "sequential jackknife"          (nf (jackknife0 mean) rs)
        , bench "standard par jackknife"      (nf (jackknife1 mean) rs)
        , bench "jackknife using Eval monad"  (nf (jackknife2 mean) rs)
        , bench "jackknife using parMap"      (nf (jackknife2b mean) rs)
        , bench "jackknife using Strategies"  (nf (jackknife3 mean) rs)
        , bench "jackknife using Par monad"   (nf (jackknife4 mean) rs)
         ]

