import Data.List (tails, transpose, elemIndices)

distance :: (Int, Int) -> (Int, Int) -> [[Char]] -> Int -> Int
distance (w, x) (y, z) xs c = f w y xs + f x z (transpose xs)
    where f :: Int -> Int -> [[Char]] -> Int
          f x y zs | x > y = (x - y) + c * length (filter (all (=='.')) 
                   $ take (x-y+1) $ drop y zs)
                   | x < y = (y - x) + c * length (filter (all (=='.'))
                   $ take (y-x+1) $ drop x zs)
                   | otherwise = 0

getPos :: [[Char]] -> [(Int, Int)]
getPos = flip f 0 
    where f [] _ = []
          f (x:xs) y = map (y,) (elemIndices '#' x) ++ f xs (y+1)

getPairs :: Int -> [(Int, Int)] -> [[Char]] -> [Int]
getPairs c xs zs = [distance x y zs c | (x:ys) <- tails xs, y <- ys]

solve1 :: [[Char]] -> Int
solve1 = sum . (getPairs 1 =<< getPos)

solve2 :: [[Char]] -> Int
solve2 = sum . (getPairs 999999 =<< getPos)

main = do
    xs <- lines <$> readFile "input.txt"
    print $ solve1 xs
    print $ solve2 xs
