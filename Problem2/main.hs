import Data.List.Split
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Applicative (Applicative(liftA2))

data Cube = R Int | B Int | G Int

correct1 :: Cube -> Bool
correct1 (R x) = x < 13
correct1 (G x) = x < 14
correct1 (B x) = x < 15

parse :: String -> [Cube]
parse = map (f . words) . splitOneOf ";,"
    where f [x, "red"]   = R $ read x
          f [x, "green"] = G $ read x
          f [x, "blue"]  = B $ read x

parseMany :: String -> [[Cube]]
parseMany = map (parse . deleteGame) . lines

deleteGame :: String -> String
deleteGame = last . splitOn ": "

value :: Cube -> Int
value (R x) = x
value (G x) = x
value (B x) = x

maxes :: [Cube] -> [Cube]
maxes xs = [ f [R x | R x <- xs], f [G x | G x <- xs], f [B x | B x <- xs]]
    where f = maximumBy (comparing value)

power :: [Cube] -> Int
power = product . map value

solve1 :: String -> Int 
solve1 = sum . map fst . filter (all correct1 . snd) . zip [1..] . parseMany

solve2 :: String -> Int 
solve2 = sum . map (power . maxes) . parseMany

main :: IO()
main = readFile "input.txt" >>=
    mapM_ print . liftA2 (:) solve1 (return . solve2)