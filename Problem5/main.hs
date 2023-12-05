import Data.List.Split

data Unrange = Unrange { dst :: Int, src :: Int, len :: Int } deriving Show
-- Intervals are [n, n)
data Interval = Interval Int Int deriving Show

offset :: Unrange -> Int
offset (Unrange dst src len) = dst - src

toInterval :: Unrange -> Interval
toInterval (Unrange d s l) = Interval s $ s + l

applyOne ::  Int -> Unrange -> Int
applyOne n u | n `within` toInterval u = n + offset u
             | otherwise               = n

within :: Int -> Interval -> Bool
within x (Interval st ed) = st <= x && x < ed

parse :: String -> ([Int], [[Unrange]])
parse s = let (sds:xs) = splitOn "\n\n" s
              f [des, src, len] = Unrange des src len
          in ( map read $ words $ drop 7 sds
             , map (map (f . map read . words)
             . tail . lines . dropWhile (/=':')) xs)

solve1 :: [Int] -> [[Unrange]] -> Int
solve1 ns us = minimum $ map
    (flip (foldl (\n us -> last $ n : filter (/=n) 
        (map (applyOne n) us))) us) ns

main = do
    s <- readFile "input.txt"
    let (sd, xs) = parse s

    print $ solve1 sd xs
    return ()