nbrFish :: Int -> [Int] -> Int
nbrFish time = sum . map (find time)
    where
        find :: Int -> Int -> Int
        find i j = table !! i !! j

        table :: [[Int]]
        table = [[tableEntry i j | j<-[0..8]] | i<-[0..256]]

        tableEntry :: Int -> Int -> Int
        tableEntry 0 0 = 1
        tableEntry i 0 = find (i - 1) 6 + find (i - 1) 8
        tableEntry 0 j = 1
        tableEntry i j = find (i - 1) (j - 1)

main :: IO ()
main =  nbrFish 80 <$> initalState >>= print >>
        nbrFish 256 <$> initalState >>= print
    where
        initalState :: IO [Int]
        initalState = map read' . words' <$> readFile "6.txt"

        words' :: String -> [String]
        words' = words . map (\c -> if c == ',' then ' ' else c)

        read' :: String -> Int
        read' s = read s