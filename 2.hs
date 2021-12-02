readInput :: IO [(String, Int)]
readInput = map (makePair . words) . lines <$> readFile "2.txt"

stringToInt :: String -> Int
stringToInt s = read s :: Int

makePair :: [String] -> (String, Int)
makePair l = (head l, (stringToInt . last) l)

calcCoord :: (Int, Int) -> [(String, Int)] -> (Int, Int)
calcCoord acc [] = acc
calcCoord (horizontal, depth) ((s, i):ps)
    | s == "forward"    = calcCoord (horizontal + i, depth) ps 
    | s == "up"         = calcCoord (horizontal, depth - i) ps 
    | s == "down"       = calcCoord (horizontal, depth + i) ps 

calcCoord' :: (Int, Int , Int) -> [(String, Int)] -> (Int, Int)
calcCoord' (horizontal, depth, aim) [] = (horizontal, depth)
calcCoord' (horizontal, depth, aim) ((s, i):ps)
    | s == "forward"    = calcCoord' (horizontal + i, depth + i * aim, aim) ps
    | s == "up"         = calcCoord' (horizontal, depth, aim - i) ps 
    | s == "down"       = calcCoord' (horizontal, depth, aim + i) ps 

main :: IO ()
main = uncurry (*) . calcCoord (0, 0) <$> readInput >>= print >>
        uncurry (*) . calcCoord' (0, 0, 0) <$> readInput >>= print
