module IMPure.Array where

declareArray :: Int -> [Int]
declareArray 0 = []
declareArray n = 0 : declareArray (n-1)

readArray :: [Int] -> Int -> Int 
readArray [] _  = error "IndexOutOfBounds"
readArray n i = n!!i

writeArray :: [Int] -> Int -> Int -> [Int]
writeArray a i x =  writeArrayInner a i x (length a)

writeArrayInner :: [Int] -> Int -> Int -> Int -> [Int]
writeArrayInner (x:xs) i y n = if length (x:xs) == (n - i)
    then y:xs else x: writeArrayInner xs i y n