import Data.List.Split (splitOn)
import Data.IntSet (fromList, IntSet, intersection, size)
import Data.Map (Map, elems, insertWith, empty, findWithDefault)

parse :: String -> (IntSet, IntSet)
parse = f . map (fromList . map read . words) . splitOn " | " . last . splitOn ": "
    where f [x, y] = (x, y)

matching :: (IntSet, IntSet) -> Int
matching = size . uncurry intersection

solve1 :: [(IntSet, IntSet)] -> Int
solve1 = sum . map ((2^) . subtract 1) . filter (>0) . map matching

solve2 :: Map Int Int -> [(Int, (IntSet, IntSet))] -> Int
solve2 m []          = sum $ elems m
solve2 m ((i, x):xs) =
    let m' = insertWith (+) i 1 m
    in solve2 (foldl 
        (\m k -> insertWith (+) k (findWithDefault 1 i m) m)
    m' [i+1..i+matching x]) xs

main = do
    s <- readFile "input.txt"

    let p = map parse $ lines s 
    print $ solve1 p
    print $ solve2 empty $ zip [1..] p