import System.Environment ( getArgs )   
import System.Exit ( ExitCode(ExitFailure), exitSuccess, exitWith, exitFailure )
import System.IO ( stdout, stderr, hPutStr ) 
import Data.Char as C ( isDigit )
import Data.List as L ( takeWhile, isPrefixOf, transpose )
import Control.Monad ( (>=>) )

message :: String
message = "d4:lastd1:0d4:datad1:0i1e1:1i0e1:21:Oeee4:prevd4:lastd1:0d4:datad1:0i2e1:1i0e1:21:Xeee4:prevd4:lastd1:0d4:datad1:0i1e1:1i1e1:21:Oeee4:prevd4:prevd4:prevd4:prevd4:prevd4:lastd1:0d4:datad1:0i0e1:1i2e1:21:Xeeee4:lastd1:0d4:datad1:0i2e1:1i1e1:21:Oeeee4:lastd1:0d4:datad1:0i2e1:1i2e1:21:Xeeee4:lastd1:0d4:datad1:0i1e1:1i2e1:21:Oeeee4:lastd1:0d4:datad1:0i0e1:1i0e1:21:Xeeeeeee"

size :: Int 
size = 3

data Player =  X | O | B deriving (Eq, Ord, Show)
type Grid = [[Player]]
type Position = (Int,Int)
type Score = Int
type Depth = Int
type Maxi = Bool
type State = Int

------main-------

main :: IO ()
main = do  
    args <- getArgs
    if length args /= 1 then exitFailure else do
    let p = parsePlayer $ head args
    msg <- getLine 
    if msg == "*"
          then do
            putStrLn ("d" ++ convertLast (0, 0, X) ++ "e")
            hPutStr stderr "Game just started. My Move {0,0,X}\n"
            exitSuccess
          else do
               case readMessage msg createEmpty of
                   Left k -> do exitWith $ ExitFailure $ read (show k) --100/101
                   Right g -> if isFull g || won g 
                                then do hPutStr stderr $ putGrid g ++ "\nI cannot perform any moves because game is already ended"
                                        exitWith $ ExitFailure 20
                                else play g p (getBestMove p g) msg

play :: Grid -> Player -> Position -> String -> IO ()
play g p (x, y) m
        | wins p grid  = do putStrLn message
                            hPutStr stderr $ myMove ++ putGrid grid ++ "\nI just performed the last move, I won"
                            exitSuccess--exitWith $ ExitFailure 10
        | isFull grid  = do putStrLn message
                            hPutStr stderr $ myMove ++ putGrid grid ++ "\nI just performed the last move, a draw"
                            exitSuccess--exitWith $ ExitFailure 12
        | otherwise    = do putStrLn message
                            hPutStr stderr $ myMove ++ putGrid grid
                            exitSuccess
        where
            message = "d" ++ convertLast (x, y, p) ++ "4:prev" ++ m ++ "e"
            myMove = "My Move {" ++ show x ++ ", " ++ show y ++ ", " ++ head (showPlayer p) ++ "}\n"
            grid = move g (convertMove x y) p

------start reading input-------

readMessage :: String -> Grid -> Either State Grid
readMessage s@('d':_) acc = 
    case split $ firstLast s of
        Left k -> Left k
        Right("", s2) -> dropKey s2 >>= (\t -> parseLast t acc >>= Right)
        Right(s1, "") -> dropKey s1 >>= (\t -> parseLast t acc >>= Right)
        Right (s1, s2) -> case getKey s1 of
                            Right ("last", t) -> dropKey s2 >>= (\prev -> parseLast t acc >>= readMessage prev)
                            Right ("prev", t) -> dropKey s2 >>= (\last -> parseLast last acc >>= readMessage t)
                            Right (_, _) -> Left 100
                            Left k -> Left k
readMessage _ _ = Left 100

parseLast :: String -> Grid -> Either State Grid --"{4:last}d1:0d4:datad1:0i2e1:1i0e1:21:Oeee" board -> Right ["BBO","BBB","BBB"]
parseLast ('d':t) acc = 
    case dropKey t of
        Left k -> Left k
        Right ('d':t1) -> 
            case getKey t1 of 
                Left k -> Left k
                Right ('d':'a':'t':'a':_, t2) -> 
                    case parseData t2 of
                        Left k -> Left k
                        Right (x, y, v, 'e':'e':s) -> 
                            case move acc (convertMove x y) (parsePlayer [v]) of
                                [] -> Left 101
                                a  -> Right a
                        Right _ -> Left 100
                Right _ -> Left 100
        Right _ -> Left 100
parseLast _ _ = Left 100

parseInt :: String -> Either State (Int, String) -- i1e...
parseInt ('i':t) = Right (read $ take number t, tail $ drop number t)
    where 
        number = length (L.takeWhile C.isDigit t)
parseInt _ = Left 100

parseValue :: String -> Either State (Char, String) -- 1:X....
parseValue ('1':':':'X':t) = Right ('X', t)
parseValue ('1':':':'O':t) = Right ('O', t)
parseValue ('1':':':_:t) = Left 100
parseValue _ = Left 100

parseData :: String -> Either State (Int, Int, Char, String) --d1:0i2e1:1i0e1:21:Oeee -> Right (2,0,'O',"ee")
parseData ('d':t) =
    do 
        (x, t2) <- dropKey t  >>= parseInt
        (y, t3) <- dropKey t2 >>= parseInt 
        (v, t4) <- dropKey t3 >>= parseValue
        return (x, y, v, tail t4)
parseData _ = Left 100

move :: Grid -> Int -> Player -> Grid
move g i c =
    if isValid g i
        then convertToGrid size (xs ++ [c] ++ ys)
        else []
    where (xs, B:ys) = splitAt i (concat g)

convertToGrid :: Int -> [a] -> [[a]]
convertToGrid n [] = []
convertToGrid n xs = take n xs : convertToGrid n (drop n xs)

dropKey :: String -> Either State String
dropKey ('i':t) = Right $ tail $ drop (length $ L.takeWhile C.isDigit t) t
dropKey s =
    let
        keyLength = L.takeWhile C.isDigit s
        postfix = drop (length keyLength) s
    in
        case length keyLength of 
            0 -> Left 100
            _ -> 
                case postfix of
                    (':':r) -> Right $ drop (read keyLength) r
                    _ -> Left 100

getKey :: String -> Either State (String, String) --key and what is left
getKey s = 
    let
        keyLength = L.takeWhile C.isDigit s
        postfix = drop (length keyLength) s
    in
        case length keyLength of 
            0 -> Left 100
            _ ->
                case postfix of
                    (':':r) -> Right (splitAt (read keyLength) r)
                    _ -> Left 100

firstLast :: String -> String
firstLast [] = []
firstLast [x] = [x]
firstLast x = tail $ init x 

split :: String -> Either State (String, String)
split s = split' s 0 >>= (\rez -> Right(remove rez s, rez))
    where
        split' "" _ = Right ""
        split' ('e':t) 0 = Right t
        split' ('e':t) index = 
            if (index - 1) == 0
                then Right t
                else split' t (index - 1)
        split' ('d':t) index = split' t (index + 1)
        split' t index = dropKey t >>= (`split'` index)

remove :: String -> String -> String --"hel", "hello" -> "lo"
remove _ "" = ""
remove "" s = s
remove w s@(c:cs) 
        | w `isPrefixOf` s = remove w (drop (length w) s)
        | otherwise = c : remove w cs


convertLast :: (Int, Int, Player) -> String 
convertLast (x, y, p) = "4:lastd1:0d4:datad1:0" ++ convertInt x ++ "1:1" ++ convertInt y ++ "1:2" ++ convertPlayer p ++ "eee"

convertInt :: Int -> String 
convertInt a = "i" ++ show a ++ "e"

convertPlayer :: Player -> String
convertPlayer p = "1:" ++ head (showPlayer p)

------end reading input-------

------start checks-------

createEmpty :: Grid
createEmpty = replicate size (replicate size B)

next :: Player -> Player
next O = X
next X = O
next B = B

convertMove :: Int -> Int -> Int -- converts 2d move to 1d -> (2 2 -> 8)
convertMove x y = x + (y * size)

isFull :: Grid -> Bool
isFull = notElem B . concat

isValid :: Grid -> Int -> Bool
isValid g i = 0 <= i && i < size^2 && concat g !! i == B 

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..(size-1)]]

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
    where
        line = all (== p)
        rows = g
        cols = L.transpose g
        dias = [diag g, diag (map reverse g)]

won :: Grid -> Bool
won g = wins O g || wins X g

------end checks-------

------start display board-------

interleave :: a -> [a] -> [a] -- 0 [1,2,3] -> [1,0,2,0,3]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

showPlayer :: Player -> [String] 
showPlayer X = ["X"]
showPlayer O = ["O"]
showPlayer B = [" "]

parsePlayer :: String -> Player
parsePlayer ['X'] = X
parsePlayer ['O'] = O
parsePlayer ['B'] = B

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer --beside $ interleave bar $ map showPlayer x
        where
            beside = foldr1 (zipWith (++)) --foldr1 (zipWith (++)) [["a1","a2"],["b1","b2"]] -> ["a1b1","a2b2"]
            bar = replicate 3 "|"

putGrid :: Grid -> String
putGrid = unlines . concat . interleave bar . map showRow
            where bar = [replicate ((size*2)-1) '-']

------end display board-------

------start minimax-------

allCells :: [Position]
allCells = [ (x2,x1) | x1 <- [0 .. size], x2 <- [0 .. size]]

freeCells :: Grid -> [Position]
freeCells xs = [(x1,x2) | x1 <- [0 .. size-1], x2 <- [0..size-1], isValid xs (convertMove x1 x2)]

isMaxi :: Player -> Bool
isMaxi X = True
isMaxi _ = False

maxPos :: [Score] -> Int
maxPos xs = foldr (\ (x,y) acc -> if x == maximum xs then y else acc) 0 (zip xs [0..])

minPos :: [Score] -> Int
minPos xs = foldr (\ (x,y) acc -> if x == minimum xs then y else acc) 0 (zip xs [0..])

getScores :: Player -> Grid -> [Position] -> [Score]
getScores p g = foldr(\(x,y) acc -> (minimax (next p) (isMaxi $ next p) 0 (move g (convertMove x y) p)) : acc) []

getBestMove :: Player -> Grid -> Position
getBestMove p g = 
      let 
          freeC = freeCells g
          scores = getScores p g freeC
      in
          if isMaxi p
              then freeC !! maxPos scores
              else freeC !! minPos scores


minimax :: Player -> Maxi -> Depth -> Grid -> Score
minimax p m d g 
    | wins O g = -10
    | wins X g = 10
    | isFull g = 0
    | otherwise =
        let 
            scores = foldr(\(x,y) acc -> (minimax (next p) (not m) (d+1) (move g (convertMove x y) p)) : acc) [] (freeCells g)
        in 
            if m 
                then maximum scores
                else minimum scores

------end minimax-------
