type Point = (Int, Int)

nums :: String -> [Int]
nums = map (\s -> read s) . words

toSpace :: Char -> Char
toSpace c = if  c == ',' || c == '-' || c == '>' then ' ' else c

makePoints :: [[Int]] -> [[Point]]
makePoints = map (\ps -> zip [ps !! 0, ps !! 2]  [ps !! 1, ps !! 3])

vertical :: [[Int]] -> [[Int]]
vertical = filter (\xs -> xs !! 1 == xs !! 3)

horizontal :: [[Int]] -> [[Int]]
horizontal = filter (\xs -> xs !! 0 == xs !! 2)

nbrOverlaps :: [Point] -> Int
nbrOverlaps [] = 0
nbrOverlaps (p:ps)
    | p `elem` ps   = 1 + nbrOverlaps (filter (/= p) ps)
    | otherwise     = nbrOverlaps ps

pointsOnLine :: [Point] -> [Point]
pointsOnLine [(x1, y1), (x2, y2)]
    | x1 == x2  && y1 < y2  = p1 : pointsOnLine [(x1, y1 + 1), p2]
    | x1 < x2   && y1 == y2 = p1 : pointsOnLine [(x1 + 1, y1), p2]
    | x1 == x2  && y1 > y2  = p1 : pointsOnLine [(x1, y1 - 1), p2]
    | x1 > x2   && y1 == y2 = p1 : pointsOnLine [(x1 -1, y1), p2]
    | x1 < x2   && y1 < y2  = p1 : pointsOnLine [(x1 + 1, y1 + 1), p2]
    | x1 < x2   && y1 > y2  = p1 : pointsOnLine [(x1 + 1, y1 - 1), p2]
    | x1 > x2   && y1 < y2  = p1 : pointsOnLine [(x1 - 1, y1 + 1), p2]
    | x1 > x2   && y1 > y2  = p1 : pointsOnLine [(x1 - 1, y1 - 1), p2]
    | p1 == p2  = [p2]
        where
            p1 = (x1, y1)
            p2 = (x2, y2)

main :: IO ()
main =  calculateOverlap . (\xss -> horizontal xss ++ vertical xss) <$>
        input >>= print >> calculateOverlap <$> input >>= print
    where
        input :: IO [[Int]]
        input = map nums . map (map toSpace) . lines <$> readFile "5.txt"

        calculateOverlap :: [[Int]] -> Int
        calculateOverlap = nbrOverlaps . foldl (++) [] .
                            map pointsOnLine . makePoints