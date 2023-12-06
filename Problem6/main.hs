parse :: String -> [[String]]
parse = map (words . dropWhile (/=' ')). lines

parse1 :: [[String]] -> [(Int, Int)]
parse1 = uncurry zip . f . (map . map) read
    where f [x, y] = (x, y)

parse2 :: [[String]] -> (Int, Int)
parse2 = f . map (read . concat)
    where f [x, y] = (x, y)

race :: (Int, Int) -> Int
race (t, d) = length $ filter (>d) $ map (\i -> i * (t-i)) [1..t]

solve1 :: [(Int, Int)] -> Int
solve1 =  product . map race

main = do
    s <- parse <$> readFile "input.txt"
    print $ solve1 $ parse1 s
    print $ race $ parse2 s