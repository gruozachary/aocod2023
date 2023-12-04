import Data.Char (isDigit, digitToInt)
import Data.Map (Map, empty, elems)
import Data.Map.Strict (insertWith)
import Control.Monad.RWS (RWS, MonadState (..), MonadReader (..), runRWS, gets, MonadWriter (..), evalRWS, liftM)
import Data.Array (Array, array, (!), Ix (inRange))
import Data.Array.Base (IArray(bounds))
import Control.Monad (unless, when, replicateM)
import Data.Set (Set, fromList)
import qualified Data.Set (foldl)

data Item = Dot | Digit Int | Symbol Bool deriving (Eq, Ord)

isItemDigit :: Item -> Bool
isItemDigit (Digit _) = True
isItemDigit _         = False

parse :: String -> [[Item]]
parse = (map . map) f . lines
    where f c | c == '.'  = Dot
              | c == '*'  = Symbol True
              | isDigit c = Digit $ digitToInt c
              | otherwise = Symbol False

parseToArray :: [[Item]] -> Array (Int, Int) Item
parseToArray xs =
    let rs = length xs - 1
        cs = length (head xs) - 1
    in array ((0, 0), (rs, cs)) [((i, j), xs !! i !! j) | i <- [0..rs], j <- [0..cs]]

data State' = State'
    { pointer           :: (Int, Int)
    , currentItem       :: Item
    , currentlyAdjacent :: [((Int, Int), Item)]
    , currentDigits     :: [Int]
    , numbers           :: [(Int, Set ((Int, Int), Item))] }

type RWS' = RWS (Array (Int, Int) Item) [String] State'

incrementPointer :: RWS' ()
incrementPointer = do
    s <- get
    r <- ask
    let (x, y)      = pointer s
        (_, (p, q)) = bounds r
        v           = x * (q+1) + y + 1
        z    = (v `div` (p+1), v `mod` (q+1))
    put s { pointer = z }

calculateAdjacency :: RWS' ()
calculateAdjacency = do
    s <- get
    r <- ask
    when (isItemDigit $ currentItem s) $ do
        let (x, y)   = pointer s
            f [x, y] = (x, y)
            ps       =
                filter (inRange (bounds r))
              $ map (f . flip (zipWith ($)) [x, y])
              $ replicateM 2 [id, (+1), subtract 1]
        put s { currentlyAdjacent =
            currentlyAdjacent s ++ [(p, Symbol x) | (p, Symbol x) <- [(p, r ! p) | p <- ps]] }

endDigit :: RWS' ()
endDigit = do
    s <- get
    unless (null $ currentlyAdjacent s) $ do
        put s { numbers =
             ( foldl ((+) . (10*)) 0 (currentDigits s)
             , fromList $ currentlyAdjacent s) : numbers s }
    s' <- get
    put s' { currentDigits = [], currentlyAdjacent = [] }

processNew :: RWS' ()
processNew = do
    s <- get
    case currentItem s of
        Digit x -> put s { currentDigits = currentDigits s ++ [x] }
        _       -> endDigit

run :: RWS' ()
run = do
    r <- ask
    s <- get
    when (inRange (bounds r) (pointer s)) $ do
        put s { currentItem = r ! pointer s }
        calculateAdjacency
        processNew
        incrementPointer
        run

getSum :: RWS' Int
getSum = do
    ns <- gets numbers
    return $ sum $ map fst ns

help :: Map (Int, Int) [Int] -> (Int, Set ((Int, Int), Item)) -> Map (Int, Int) [Int]
help m (n, xs) = foldl f m xs
    where f m (p, Symbol True) = insertWith (++) p [n] m
          f m (p, _)           = m

gearRatio :: RWS' Int
gearRatio = do
    ns <- gets numbers
    return $ sum $ map product
           $ filter ((==2) . length)
           $ elems $ foldl help empty ns

main = do
    e <- parseToArray . parse <$> readFile "input.txt"
    let f = fst . flip (`evalRWS` e) (State' (0, 0) Dot [] [] [])
    print $ f (run >> getSum)
    print $ f (run >> gearRatio)