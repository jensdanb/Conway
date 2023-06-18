module Gen (nextGen) where


import Data.List (singleton, tails, transpose)


nextGen :: [[Int]] -> [[Int]]
nextGen grid = zipWith innerZip grid (neighborCount grid)
    where innerZip = zipWith evalLife
          evalLife cell nLiveCells
              | cell == 0 && nLiveCells == 3                    = 1
              | cell == 1 && nLiveCells == 3 || nLiveCells == 4 = 1
              | otherwise                                       = 0



neighborCount :: [[Int]] -> [[Int]]
neighborCount grid = [topRow] ++ midRows ++ [bottomRow]
    where topRow    = sumSubGrids $ take 2 grid
          bottomRow = sumSubGrids $ take 2 (reverse grid)
          midRows   = map sumSubGrids $ triples grid


sumSubGrids :: [[Int]] -> [Int]
sumSubGrids = map sum . transpose . map rowSum


-- ASSUMES length input >= 3
-- Example: [0,0,1,1,2] -> [0,1,2,4,3]
rowSum :: [Int] -> [Int]
rowSum row = leftEnd ++ middle ++ rightEnd
    where leftEnd = cornerGen $ take 2 row
          rightEnd = cornerGen $ take 2 (reverse row)
          middle = midGen row

          cornerGen = singleton . sum
          midGen =  map sum . triples


-- ASSUMES length input >= 3
-- Example1: [0,1,2,3,4] -> [[0,1,2], [1,2,3], [2,3,4]]
-- Example2: [rowA, rowB, rowC, rowD] -> [[rowA, rowB, rowC], [rowB, rowC, rowD]]
-- Notice that length reduced by 2
triples :: [a] -> [[a]]
triples = dropLast 3 . map (take 3) . tails
    where dropLast 0 xs = xs
          dropLast n xs = dropLast (n-1) (init xs)



-- 1D version below --
nextGen1D :: [Int] -> [Int]
nextGen1D = (map evalLife) . rowSum
    where evalLife 1 = 1
          evalLife _ = 0
