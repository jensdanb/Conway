module Main where

import Data.List (intersperse)
import Launcher (printBuildInfo, getArrayDimensions, buildGrid, getGenerationLimit)
import Gen (nextGen)


main :: IO ()
main = do
  printBuildInfo

  gridDims <- getArrayDimensions
  generations <- getGenerationLimit
  let grid = buildGrid gridDims

  putStrLn "\n Generation 0"
  printGrid grid

  generateNext grid generations 1


-- g=grid, n=(remaining generations), c=(current generation)
generateNext g 0 c = do putStrLn $ "Finished at generation " ++ show (c-1)
generateNext g n c = do
  -- Generate next gen. use `seq` to evaluate and avoid memory leak.
  let g' = nextGen g  -- Add {g `seq`} right after = sign to strictly evaluate
  putStrLn $ "\n Generation " ++ show c
  printGrid g'

  generateNext g' (n-1) (c+1)


printGrid :: [[Int]] -> IO ()
printGrid [] = do putStrLn "\n"
printGrid (row:rows) = do
  putStrLn (printRow row)
  printGrid rows

  where printRow row = intersperse ' ' [if x == 1 then 'X' else '.' | x <- row]
