module IMPure.Array where

declareArray :: Int -> [Int]
declareArray 0 = []
declareArray n = 0 : declareArray (n-1)

readArray :: [Int] -> Int -> Int 
readArray [] _  = error "IndexOutOfBounds"
readArray n i = n!!i

writeArray :: [Int] -> Int -> Int -> [Int]
writeArray (a:as) 0 x =  x : as
writeArray (a:as) i x =  a : writeArray as (i-1) x
