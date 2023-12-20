import Data.List (tails, transpose, elemIndices)

distance :: (Int, Int) -> (Int, Int) -> [[Char]] -> Int
distance (w, x) (y, z) xs = f w y xs + f x z (transpose xs)
    where f :: Int -> Int -> [[Char]] -> Int
          f x y zs | x > y = (x - y) + 999999 * length (filter (all (=='.')) 
                   $ take (x-y+1) $ drop y zs)
                   | x < y = (y - x) + 999999 * length (filter (all (=='.'))
                   $ take (y-x+1) $ drop x zs)
                   | otherwise = 0

getPos :: [[Char]] -> [(Int, Int)]
getPos = flip f 0 
    where f [] _ = []
          f (x:xs) y = map (y,) (elemIndices '#' x) ++ f xs (y+1)

getPairs :: [(Int, Int)] -> [[Char]] -> [Int]
getPairs xs zs = [distance x y zs | (x:ys) <- tails xs, y <- ys]

main = do
    xs <- lines <$> readFile "input.txt"
    print $ sum $ getPairs (getPos xs) xs
