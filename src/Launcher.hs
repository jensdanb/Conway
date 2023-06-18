module Launcher (printBuildInfo, getArrayDimensions, buildGrid, getGenerationLimit) where


import System.Directory (listDirectory)
import Data.Char (isDigit)


printBuildInfo :: IO ()
printBuildInfo = do
    contents <- listDirectory "src"
    putStrLn ("Source files:" ++ show contents ++ "\n")


-- ASSUMES iMin < iMax
-- External function must provide appropriate iMin and iMax for its use.
safeUserInt :: (Int, Int) -> IO Int
safeUserInt (iMin, iMax) = do
    userInput <- getLine
    returnOrRetry userInput
        where returnOrRetry input
                | all isDigit input && intValue `elem` [iMin..iMax] = return intValue
                | otherwise                                  = safeUserInt (iMin, iMax)
                where intValue = read input :: Int


getArrayDimensions :: IO (Int, Int)
getArrayDimensions = do
    putStrLn $ "Enter horizontal size of array. Number between 3 and 40"
    xSize <- safeUserInt (3, 40)
    putStrLn $ "Enter vertical size of array. Number between 3 and 40"
    ySize <- safeUserInt (3, 40)
    return (xSize, ySize)


buildGrid :: (Int, Int) -> [[Int]]
buildGrid (x, y) = replicate y (seed x)


-- assumes 3 <= length <= 40
seed :: Int -> [Int]
seed x = take x preShuffledList
    where preShuffledList =
            [0,0,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1]


getGenerationLimit :: IO Int
getGenerationLimit = do
    putStrLn $ "Stop at generation number n. Number between 1 and 100 000"
    n <- safeUserInt (1, 100000)
    return n
