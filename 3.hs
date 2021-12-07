import Data.Char

count :: String -> Int
count = sum . map (\c-> if c == '1' then 1 else -1)

binToDec :: Int -> [Int] -> Int
binToDec exp bits
    | exp >= length bits    = 0
    | otherwise             = (bits !! exp) * (2 ^ exp) +
                                binToDec (exp + 1) bits

gamma :: [String] -> [Int]
gamma [] = []
gamma ss = if and . map null $ ss then [] else bit : gamma (map tail ss)
    where
        bit = if (count . map head $ ss) > 0 then 1 else 0

epsilon :: [String] -> [Int]
epsilon = map (\x -> if x == 1 then 0 else 1) . gamma

oxygen :: Int -> [String] -> [Int]
oxygen _ [s] = map digitToInt s
oxygen pos ss = oxygen (pos + 1) (filter (\s -> s !! pos == bit) ss)
    where
        bit = if (count . map (!! pos) $ ss) >= 0 then '1' else '0'

co2 :: Int -> [String] -> [Int]
co2 _ [s] = map digitToInt s
co2 pos ss = co2 (pos + 1) (filter (\s -> s !! pos == bit) ss)
    where
        bit = if (count . map (!! pos) $ ss) >= 0 then '0' else '1'

main :: IO ()
main = do
    input <- lines <$> readFile "3.txt"
    print . foldl (*) 1 . map (binToDec 0 . reverse) $
        [gamma input, epsilon input]
    print . foldl (*) 1 . map (binToDec 0 . reverse) $
        [oxygen 0 input, co2 0 input]