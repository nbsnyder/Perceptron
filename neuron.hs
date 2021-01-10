{-# LANGUAGE ForeignFunctionInterface #-}

{-
This code runs a perceptron learning algorithm on a list of inputs and outputs. It will determine
a list of weights/coefficients that form a line that separates the data as best as it can. This code
will be called from C code using the getWeights function. It also requires two helper functions from
the C code: c_readVal to read a value from a C array and c_writeVal to write a value to a C array.
-}

module Neuron where

import Foreign.Ptr (Ptr)

-- Read a value from a C array
-- This function is defined in the C code as readFromDblArr(double*, int)
-- Parameters: (1) pointer to the C array of type double*, (2) index of the array
foreign import ccall unsafe "readFromDblArr" c_readVal :: Ptr Double -> Int -> Double

-- Write a single value to a C array
-- This function is defined in the C code as writeToDblArr(double*, int, double)
-- Parameters: (1) pointer to the C array of type double*, (2) index of the array, (3) value to write
foreign import ccall unsafe "writeToDblArr" c_writeVal :: Ptr Double -> Int -> Double -> IO()

-- Convert a section of a C array to a Haskell list of type [Double]
-- Parameters: (1) pointer to the C array of type double*, (2) start of the subarray, (3) length of the subarray
readCArrToList :: Ptr Double -> Int -> Int -> [Double]
readCArrToList ptr start length = map (c_readVal ptr . (+ start)) [0..(length-1)]

-- Convert an entire 2D C array to a Haskell list of type [[Double]]
-- Parameters: (1) pointer to the C array of type double*, (2) major length of the array, (3) minor length of the array
readCArrArrToList :: Ptr Double -> Int -> Int -> [[Double]]
readCArrArrToList ptr size1 size2 = map ((\i -> readCArrToList ptr i size2) . (* size2)) [0..(size1 - 1)]

-- Write an entire Haskell list to a C array by repeatedly calling c_writeVal
-- Parameters: (1) pointer to the C array of type double*, (2) index of the array, (3) haskell list to write
writeListToCArr :: Ptr Double -> Int -> [Double] -> IO()
writeListToCArr ptr start hlist
    | null hlist = return ()
    | otherwise = do
        c_writeVal ptr start (head hlist)
        writeListToCArr ptr (start + 1) (tail hlist)

-- Get the dot product of two lists
getDotProduct :: [Double] -> [Double] -> Double
getDotProduct arr1 arr2 = sum $ zipWith (*) arr1 arr2

-- Change the weights list based on the input and output
-- Return the updated weights list
changeWeight :: [Double] -> [Double] -> Double -> [Double]
changeWeight input weights output
    | (output * getDotProduct input weights) > 0 = weights
    | otherwise = zipWith (+) weights $ map (* learningRateMultiplier) input
        where
            -- The learning rate (between 0 and 1) determines how fast the model adjusts itself (output controls the sign)
            learningRateMultiplier = 0.5 * output

-- Build up the list of weights by changing the weights for each input
-- Return the final weights list
buildWeightsList :: [[Double]] -> [Double] -> [Double] -> [Double]
buildWeightsList inputs weights outputs
    | null outputs = weights
    | otherwise = buildWeightsList (tail inputs) newWeights (tail outputs)
        where
            newWeights = changeWeight (head inputs) weights (head outputs)

-- Find the optimal weights given input and output arrays and write them to the weights array
-- This function is exported to be called from the C code
-- Parameters: (1) the inputs array (C pointer to 2D array), (2) the weights array (double*), (3) the outputs array (double*)
-- Parameters (cont.): (4) the major length of the inputs array, (5) the minor length of the inputs array
getWeights :: Ptr Double -> Ptr Double -> Ptr Double -> Int -> Int -> IO()
getWeights inputs weights outputs size1 size2 = writeListToCArr weights 0 $ buildWeightsList inputsArr zerosArr outputsArr
    where
        -- Retrieve the inputs array and format it into a Haskell list of type [[Double]]
        inputsArr = readCArrArrToList inputs size1 size2
        -- Start the weights as a list of all 0s
        zerosArr = replicate size2 0
        -- Retrive the outputs array and format it into a Haskell list of type [Double]
        outputsArr = readCArrToList outputs 0 size1
foreign export ccall getWeights :: Ptr Double -> Ptr Double -> Ptr Double -> Int -> Int -> IO()
