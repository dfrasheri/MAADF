{-# LANGUAGE FlexibleContexts #-}

import Numeric.LinearAlgebra
import System.Random
import Data.List
import Control.Monad (forM)
import Data.Function (on)

-----------------------------
-- Data Generation Section --
-----------------------------

-- Generate synthetic linear data: y = a * x + b + noise
generateData :: Int -> IO [(Double, Double)]
generateData n = do
    gen <- newStdGen
    let xs = take n $ randomRs (-10, 10) gen
    let noise = take n $ randomRs (-1, 1) gen
    let ys = zipWith (\x e -> 3 * x + 5 + e) xs noise
    return $ zip xs ys

-- Convert (x, y) pairs to input matrix X and target vector y
toMatrix :: [(Double, Double)] -> (Matrix Double, Vector Double)
toMatrix xyPairs = (fromLists $ map (\(x, _) -> [1, x]) xyPairs, fromList $ map snd xyPairs)

-------------------------------
-- Linear Regression Section --
-------------------------------

-- Train linear regression using the normal equation: (X^T X)^(-1) X^T y
trainLinearRegression :: Matrix Double -> Vector Double -> Vector Double
trainLinearRegression x y = (inv (tr x <> x)) <> tr x #> y

-- Predict using learned weights
predict :: Matrix Double -> Vector Double -> Vector Double
predict x weights = x #> weights

---------------------------
-- Cross-Validation Part --
---------------------------

-- Split data into k roughly equal parts
kFoldSplit :: Int -> [a] -> [[a]]
kFoldSplit k xs = go xs (length xs `div` k)
  where
    go [] _ = []
    go l size = let (h, t) = splitAt size l in h : go t size

-- Shuffle a list randomly
shuffle :: [a] -> IO [a]
shuffle xs = do
    gen <- newStdGen
    return $ map snd $ sortBy (compare `on` fst) $ zip (randoms gen :: [Int]) xs

-- Evaluate MSE
mse :: Vector Double -> Vector Double -> Double
mse yTrue yPred = sumElements ((yTrue - yPred) ** 2) / fromIntegral (size yTrue)

-- Perform k-fold cross-validation
crossValidate :: Int -> [(Double, Double)] -> IO [Double]
crossValidate k dataset = do
    shuffled <- shuffle dataset
    let folds = kFoldSplit k shuffled
    forM [0..k-1] $ \i -> do
        let testSet = folds !! i
        let trainSet = concat $ take i folds ++ drop (i+1) folds
        let (xTrain, yTrain) = toMatrix trainSet
        let (xTest, yTest) = toMatrix testSet
        let weights = trainLinearRegression xTrain yTrain
        let yPred = predict xTest weights
        return $ mse yTest yPred

------------------------
-- Pretty Print Tools --
------------------------

formatDouble :: Double -> String
formatDouble = show . (/1.0) . fromIntegral . round . (* 100000)

printResults :: [Double] -> IO ()
printResults mses = do
    putStrLn "Fold-wise MSE:"
    mapM_ (putStrLn . formatDouble) mses
    let avg = sum mses / fromIntegral (length mses)
    putStrLn $ "Average MSE: " ++ formatDouble avg

--------------------------
-- Main Evaluation Flow --
--------------------------

main :: IO ()
main = do
    putStrLn "Generating data..."
    dataPoints <- generateData 100

    putStrLn "Performing 5-fold cross-validation..."
    results <- crossValidate 5 dataPoints

    printResults results
