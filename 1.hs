readInput :: IO [Int]
readInput = map (\s -> read s :: Int) . words <$> readFile "1.txt"

nbrOfIncs :: [Int] -> Int
nbrOfIncs [] = 0
nbrOfIncs xs = length . filter (> 0) . zipWith (-) (tail xs) $ init xs

slidingWindow :: Int -> [Int] -> [[Int]]
slidingWindow n l
    | n > length l = []
    | otherwise     = take n l : slidingWindow n (drop 1 l)

-- Part 1
main1 :: IO Int
main1 = nbrOfIncs <$> readInput

-- Part 2
main2 :: IO Int 
main2 = nbrOfIncs . map (foldl (+) 0) . slidingWindow 3 <$> readInput