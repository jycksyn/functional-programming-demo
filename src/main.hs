import GHC.Float (divideFloat, divideDouble)
-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage = print

-- Write division here
division :: Double -> Double -> Maybe Double
division x y
    | y == 0 = Nothing
    | otherwise = Just (divideDouble x y)

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

-- Write factList here
factList :: Int -> [Int]
factList 1 = [1]
factList 2 = [1,2]
factList n = xs ++ [last xs * n] where xs = factList (n-1)

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge l1 [] = l1
merge [] l2 = l2
merge l1 l2
    | head l1 < head l2 = head l1 : merge (tail l1) l2
    | otherwise = head l2 : merge l1 (tail l2)

main = print (merge [1, 3, 6] [2, 4, 5, 6, 7])