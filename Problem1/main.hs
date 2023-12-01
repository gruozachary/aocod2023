import Data.Char (isDigit, digitToInt)
import Control.Monad.State
import Data.List (isPrefixOf)
import Control.Arrow (Arrow(first))
import Control.Applicative (liftA2)

type MyState = State (String, [Int])
type Checker = String -> Maybe Int

check1 :: Checker
check1 xs = if isDigit $ head xs
    then Just $ digitToInt $ head xs
    else Nothing

check2 :: Checker
check2 xs | isDigit $ head xs = Just $ digitToInt $ head xs
          | reverse "one"   `isPrefixOf` xs = Just 1
          | reverse "two"   `isPrefixOf` xs = Just 2
          | reverse "three" `isPrefixOf` xs = Just 3
          | reverse "four"  `isPrefixOf` xs = Just 4
          | reverse "five"  `isPrefixOf` xs = Just 5
          | reverse "six"   `isPrefixOf` xs = Just 6
          | reverse "seven" `isPrefixOf` xs = Just 7
          | reverse "eight" `isPrefixOf` xs = Just 8
          | reverse "nine"  `isPrefixOf` xs = Just 9
          | otherwise                       = Nothing

check :: Checker -> MyState ()
check f = do
    (s, is) <- get
    put (s, case f s of
            Just x  -> x:is
            Nothing -> is)

run :: Checker -> String -> MyState Int
run _ [] = do
    is <- gets snd
    return $ if null is
        then 0
        else (last is * 10) + head is
run f (x:xs) = do
    modify (first (x:))
    check f
    run f xs

solve :: Checker -> String -> String
solve f s = show $ sum $ map (\p -> evalState (run f p) ("", [])) $ lines s

solve1 :: String -> String
solve1 = solve check1

solve2 :: String -> String
solve2 = solve check2

main = readFile "input.txt" >>=
    mapM_ putStrLn . liftA2 (:) solve1 (return . solve2)