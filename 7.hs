cost :: [Int] -> Int -> Int
cost [] _ = 0
cost (x:xs) pos = abs (pos - x) + cost xs pos 

cost' :: [Int] -> Int -> Int
cost' [] _ = 0
cost' (x:xs) pos = sum [1 .. abs (pos - x)] + cost' xs pos 

minimalCost :: ([Int] -> Int -> Int) -> [Int] -> Int
minimalCost cost xs = minimum . map (cost xs) $ [0 .. maximum xs]

main :: IO ()
main =  input >>= print . minimalCost cost >> input >>=
        print . minimalCost cost'
    where
        input :: IO [Int]
        input = map read' . words . map (\c -> if c == ',' then ' ' else c) <$>
                readFile "7.txt"

        read' :: String -> Int
        read' = read