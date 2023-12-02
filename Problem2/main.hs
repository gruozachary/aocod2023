import Data.List.Split
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Applicative (Applicative(liftA2))

data Cube = Red Int | Blue Int | Green Int deriving Show

parseChunk :: [String] -> Cube
parseChunk [x, "blue"]  = Blue $ read x
parseChunk [x, "green"] = Green $ read x
parseChunk [x, "red"]   = Red $ read x

correct1 :: Cube -> Bool
correct1 (Red x)   = x < 13
correct1 (Green x) = x < 14
correct1 (Blue x)  = x < 15

fullParse :: String -> [Cube]
fullParse = map (parseChunk . words) . splitOneOf ";,"

parseMany :: String -> [[Cube]]
parseMany = map (fullParse . deleteGame) . lines

deleteGame :: String -> String
deleteGame = last . splitOn ": "

value :: Cube -> Int
value (Red x)   = x
value (Green x) = x
value (Blue x)  = x

maxes :: [Cube] -> [Cube]
maxes xs = [ maximumBy (comparing value) [Red x | Red x <- xs]
           , maximumBy (comparing value) [Green x | Green x <- xs]
           , maximumBy (comparing value) [Blue x | Blue x <- xs] ]

power :: [Cube] -> Int
power = product . map value

solve1 :: String -> Int 
solve1 = sum . map fst . filter (all correct1 . snd) . zip [1..] . parseMany

solve2 :: String -> Int 
solve2 = sum . map (power . maxes) . parseMany

main :: IO()
main = readFile "input.txt" >>=
    mapM_ print . liftA2 (:) solve1 (return . solve2)