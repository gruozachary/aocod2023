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

run1 :: [Instruction] -> InsMap -> Int
run1 is m = f (cycle is) 0 "AAA"
    where f :: [Instruction] -> Int -> String -> Int
          f _ n "ZZZ"         = n
          f (RightDir:is) n s = f is (n+1) (snd $ findWithDefault ("", "") s m)
          f (LeftDir:is)  n s = f is (n+1) (fst $ findWithDefault ("", "") s m)

main = do
    s <- readFile "input.txt"
    print $ uncurry run1 $ parse s
