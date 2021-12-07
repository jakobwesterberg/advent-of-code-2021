import Data.Maybe
import Data.List

type Board = [[Int]]

nums :: String -> [Int]
nums = map (\s -> read s :: Int ) . words

makeBoards :: [String] -> [Board]
makeBoards [] = []
makeBoards ss = (map nums . takeWhile (/= "")) ss :
                makeBoards ((drop 1 . dropWhile (/= "")) ss)

columnVicroty :: Board -> Bool
columnVicroty = rowVictory . transpose

rowVictory :: Board -> Bool
rowVictory b 
    | and . map (== -1) . head $ b  = True
    | length b == 1                 = False
    | otherwise                     = rowVictory (tail b)

score :: Board -> Int
score = sum . map sum . map (filter (/= -1))

hasBingo :: Board -> Maybe Int
hasBingo b =    if rowVictory b || columnVicroty b then
                    Just (score b)
                else
                    Nothing

updateBoards :: Int -> ([Board] -> [Board])
updateBoards x = map $ map $ map $ \a -> if a == x then -1 else a

head' :: [Maybe a] -> Maybe a
head' []    = Nothing
head' xs    = head xs

play :: [Int] -> [Board] -> Maybe Int
play (x:xs) bs =    if (or . map hasBingo) (updateBoards x bs) then
                        (<$>) (* x) . head' . filter isJust . map hasBingo $
                        updateBoards x bs
                    else
                        play xs $ updateBoards x bs
    where
        or :: [Maybe a] -> Bool
        or [] = False
        or (x:xs) = if isJust x then True else or xs

play' :: [Int] -> [Board] -> Maybe Int
play' [] _      =   Nothing
play' _ []      =   Nothing
play' (x:xs) bs =   pick ((<$>) (* x) . head' . filter isJust . map hasBingo $
                    updateBoards x bs) 
                    (play' xs $ filter (isNothing . hasBingo) $
                    updateBoards x bs)
    where
        pick :: Maybe a -> Maybe a -> Maybe a
        pick x y
            | isNothing x   = y
            | isNothing y   = x
            | otherwise     = y

main :: IO ()
main = do
    input <- lines <$> readFile "4.txt"
    let numbers = nums . map (\c -> if c == ',' then ' ' else c) . head $ input
    let boards = makeBoards $ drop 2 input
    print . fromJust . play numbers $ boards
    print . fromJust . play' numbers $ boards