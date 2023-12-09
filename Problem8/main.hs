import Data.Map (Map, insert, empty, findWithDefault, elems, filterWithKey, keys)
data Instruction = RightDir | LeftDir deriving (Show, Eq)
type InsMap      = Map String (String, String)

parseToMap :: [String] -> InsMap
parseToMap = foldl f empty
    where f :: InsMap -> String -> InsMap
          f m s = insert (take 3 s) (drop 7 $ take 10 s, drop 12 $ take 15 s) m

parse :: String -> ([Instruction], InsMap)
parse s = let ls = lines s
              f 'R' = RightDir
              f 'L' = LeftDir
          in (map f $ head ls, parseToMap $ drop 2 ls)

cycleLength :: [Instruction] -> InsMap -> (String -> Bool) -> String -> Int
cycleLength is m g = f (cycle is) 0 
    where f :: [Instruction] -> Int -> String -> Int
          f (i:is) n s | g s = n
                       | otherwise = f is (n+1) (h i $ findWithDefault ("", "") s m)
          h RightDir = snd
          h LeftDir  = fst

run1 :: [Instruction] -> InsMap -> Int
run1 is m = cycleLength is m (=="ZZZ") "AAA"

run2 :: [Instruction] -> InsMap -> Int
run2 is m = foldl lcm 1 $ map 
    (cycleLength is m ((=='Z' ) . last))
    (keys $ filterWithKey (\k _ -> last k == 'A') m)

main = do
    s <- readFile "input.txt"
    print $ uncurry run1 (parse s)
    print $ uncurry run2 (parse s)
