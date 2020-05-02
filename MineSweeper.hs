module MineSweeper where
import Data.Char
import Data.List.Split
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen

data State = Opened | Closed | Marked
                deriving (Eq, Show)

data Value = Numeric Int | Bomb
                deriving (Eq, Show)

data Cell = Cell { state :: State, value :: Value }
                deriving (Eq, Show)

data GameField = GameField { rows :: [[Cell]] }
                deriving (Eq, Show)

data Action = Click | Mark | Invalid
                deriving (Eq, Show)

type Pos = (Int, Int)


data Interface = Interface 
    {   iNewGame    :: Int -> Int -> Int -> StdGen -> GameField 
    ,   iMarkCell   :: GameField -> Pos -> GameField
    ,   iClickCell  :: GameField -> Pos -> GameField
    ,   iHasWon     :: GameField -> Bool
    ,   iGameOver   :: GameField -> Bool
    }

runGame :: Interface -> IO ()
runGame i = do
  putStrLn "MineSweeper Functional Programming Project"
  putStrLn "Enter column of the game"
  x <- getLine 
  putStrLn "Enter row of the game"
  y <- getLine
  putStrLn "Enter number of bombs"
  bombs <- getLine
  let xint = read x :: Int
  let yint = read y :: Int
  let nbrBombs = read bombs :: Int
  g <- newStdGen
  gameLoop i (iNewGame i xint yint nbrBombs g)

gameLoop :: Interface -> GameField -> IO ()
gameLoop i gameField = do
    printField gameField
    if (iGameOver i gameField) || (iHasWon i gameField) then do
        finish i (iHasWon i gameField)
    else do
        putStrLn "Click [C] or Mark [M] a position, [C y x] or [M y x]"
        inputLine <- getLine
        let (action, pos) = parseInput inputLine
        if (isValidInput action pos gameField) then do 
            if action == Click then do
                gameLoop i (iClickCell i gameField pos)
            else do
                gameLoop i (iMarkCell i gameField pos)
        else do
            putStrLn ("Invalid input")
            gameLoop i gameField

isValidInput :: Action -> Pos -> GameField -> Bool
isValidInput Invalid _ _                = False
isValidInput _ (y, x) (GameField rows)  = not (y < 0 || y >= yMax || x < 0 || x >= xMax)
    where 
        yMax = length rows
        xMax = length (rows !! 0)

parseInput :: String -> (Action, Pos)
parseInput s = 
    if valid then
        if head inputs == "M" then
            (Mark, pos)
        else if head inputs == "C" then
            (Click, pos)
        else
            (Invalid, pos)
    else
        (Invalid, (-1,-1))
    where 
        inputs = splitOn " " s
        valid = length inputs == 3
        pos = (read (inputs !! 1) :: Int, read (inputs !! 2) :: Int)

finish :: Interface -> Bool -> IO ()
finish i didWin = do
    if didWin 
        then 
            putStrLn ("YOU WON")
        else
            putStrLn("YOU LOST")

printField :: GameField -> IO ()
printField (GameField rows) = do
        putStrLn ( foldl (++) "    " [digs i ++ " " | (i, _) <- zip [0..] (rows !! 0)])   
        putStrLn ( foldl (++) "    " ["___" | _ <- (rows !! 0)])   
        putStrLn (unlines [ foldl (++) ((digs rowNum) ++ [' ', '|', ' ']) [[cellToChar c] ++ "  " | c <- row ] | (rowNum, row) <- zip [0..] rows] )

cellToChar :: Cell -> Char
cellToChar (Cell Closed _)              = '.'
cellToChar (Cell Marked _)             = '@'
cellToChar (Cell Opened (Numeric 0))    = ' ' 
cellToChar (Cell _ (Numeric n))         = intToDigit n
cellToChar _                            = '+'  

digs :: Int -> [Char]
digs 0 = ['0', '0']
digs x = 
    if x < 10 then 
        ['0'] ++ digs' x
    else
        digs' x

digs' :: Int -> [Char]
digs' 0 = []
digs' x = digs' (x `div` 10) ++ [intToDigit (x `mod` 10)]    

-- Just an example game field.
example = GameField [
    [ec, ec, ec, ec, ec], 
    [e1, e1, e1, ec, ec],
    [e1, bo, e2, e1, ec],
    [e1, e2, bo, e1, ec],
    [ec, e1, e1, e1, ec]]
    where 
        ec = Cell Closed (Numeric 0)
        e1 = Cell Closed (Numeric 1)
        e2 = Cell Closed (Numeric 2)
        bo = Cell Closed Bomb

posOffset = [(-1,-1), (-1,0), (-1,1),
             (0, -1),         ( 0,1),
             (1, -1), ( 1,0), ( 1,1)]

-- Instance for an arbitrary game field
instance Arbitrary GameField where 
    arbitrary = do
        maxX <- elements [5..10]
        maxY <- elements [5..10]
        bombs <- elements [1..10]
        seed <- elements [1..999999]
        let gen = mkStdGen seed
        return (newGame maxX maxY bombs gen)

-- Generates a new game field with the given parameters
newGame :: Int -> Int -> Int -> StdGen -> GameField
newGame sizeX sizeY noBombs gen = addNumerics bombField bombPos
    where
        -- Start from an empty game field
        empty = emptyGameField sizeY sizeX
        -- Then add bombs at random positions
        bombPos = nRandPos gen noBombs [] (sizeY, sizeX)
        bombField = addBombs empty bombPos

-- Adds bombs to the given positions in the game field
addBombs :: GameField -> [Pos] -> GameField
addBombs gF [] = gF
addBombs (GameField rows) ((y,x):xs) = addBombs gF' xs
    where 
        gF' = GameField (rows !!= (y, rows !! y !!= (x, (Cell Closed Bomb))))

-- Updates the cells around all bombs to show correct number
addNumerics :: GameField -> [Pos] -> GameField
addNumerics gF []                           = gF
addNumerics (GameField rows) (pos:postail)  = addNumerics gF postail
    where
        gF = addNumerics' (GameField rows) $ calcOffsetPos (GameField rows) pos

addNumerics' :: GameField -> [Pos] -> GameField
addNumerics' gF [] = gF
addNumerics' (GameField rows) ((y,x):postail) =
    if val == Bomb then
        addNumerics' (GameField rows) postail
    else
        addNumerics' gF' postail
    where 
        (Cell state val) = rows !! y !! x
        (Numeric v) = val
        gF' = GameField (rows !!= (y, rows !! y !!= (x, (Cell state (Numeric(v+1))))))

-- Creates an empty game field of given dimensions
emptyGameField :: Int -> Int -> GameField
emptyGameField maxY maxX = GameField [ row | _ <- [0..maxY]]
    where 
        row = [Cell Closed (Numeric 0) | _ <- [0..maxX]]

-- Generates n random positions in the game field
nRandPos :: StdGen -> Int -> [Pos] -> Pos -> [Pos]
nRandPos _ 0 list _ = list 
nRandPos gen n list (maxY, maxX) = 
    if or [x == x' && y == y' | (y', x') <- list] then
        nRandPos gen'' n list (maxY, maxX)
    else 
        nRandPos gen'' (n-1) (list ++ [(y,x)]) (maxY, maxX)
    where
        (y, gen') = randomR (0, maxY) gen
        (x, gen'') = randomR (0, maxX) gen'

-- Flags a cell with the given position
markCell :: GameField -> Pos -> GameField
markCell (GameField rows) (y,x) = 
    if isOpened (rows !! y !! x) then
        (GameField rows)
    else if isMarked (rows !! y !! x) then
        GameField (rows !!= (y, rows !! y !!= (x, (Cell Closed v))))
    else
        GameField (rows !!= (y, rows !! y !!= (x, (Cell Marked v))))
    where
        (Cell _ v) = rows !! y !! x

prop_markCell :: GameField -> Pos -> Property
prop_markCell (GameField rows) (y,x) = not (isOpened (Cell s v)) ==>
        if isMarked (Cell s v) then
            s' == Closed
        else
            s' == Marked
    where 
        y' = y `mod` (length rows)
        x' = x `mod` (length (rows !! y'))
        (Cell s v) = rows !! y' !! x'
        (GameField rows') = markCell (GameField rows) (y',x')
        (Cell s' v') = rows' !! y' !! x'

-- Updates a list at the given index with the given value
(!!=) :: [a] -> (Int,a) -> [a]
list !!= (i, v) = [ if index == i then v else value | 
    (index, value) <- zip [0..] list]

-- Prop for !!=
prop_update_list :: Eq a => [a] -> (Int, a) -> Bool
prop_update_list list (pos, v) | l == 0 = True
                                | otherwise = l == (length updated) 
                                    && list !! pos' == v 
                                    && and [updated !! p == list !! p | 
                                        (p, val) <- zip[0..] list, p /= pos']
    where 
        l = length list
        updated = list !!= (pos', v)
        pos' = pos `mod` l

isOpened :: Cell -> Bool
isOpened (Cell Opened _)    = True
isOpened _                  = False

isMarked :: Cell -> Bool
isMarked (Cell Marked _)  = True
isMarked _                 = False

isEmptyCell :: Cell -> Bool
isEmptyCell (Cell _ (Numeric 0)) = True
isEmptyCell _                    = False 

-- Opens a cell at a given position if it isn't Marked.
clickCell :: GameField -> Pos -> GameField
clickCell (GameField rows) (y,x) =
    if (isOpened (Cell state v) || isMarked (Cell state v)) then 
        (GameField rows)
    else 
        if (isEmptyCell (Cell state v)) then
            -- Recursively open neigboring cells if this cell is completely empty
            clickCell' clickedGf (calcOffsetPos clickedGf (y,x))
        else
            clickedGf
    where 
        clickedGf       = (GameField (rows !!= (y, rows !! y !!= (x, (Cell Opened v)))))
        (Cell state v) = rows !! y !! x

clickCell' :: GameField -> [Pos] -> GameField
clickCell' gf [pos]         = clickCell gf pos
clickCell' gf (pos:posxs)   = clickCell' gf' posxs
    where
        gf' = clickCell gf pos

prop_clickCell :: GameField -> Pos -> Property
prop_clickCell (GameField rows) (y,x) = 
    not (isOpened (Cell s v)) && not (isEmptyCell (Cell s v)) ==>
        s' == Opened && v' == v
    where 
        y' = y `mod` (length rows)
        x' = x `mod` (length (rows !! y'))
        (Cell s v) = rows !! y' !! x'
        (GameField rows') = clickCell (GameField rows) (y',x')
        (Cell s' v') = rows' !! y' !! x'

-- Calculates all surrounding positions of a given coordinate
calcOffsetPos :: GameField -> Pos -> [Pos]
calcOffsetPos (GameField rows) (y,x) = 
    [(y'',x'') | (y',x') <- posOffset, let y'' = y+y', let x'' = x+x', y'' >= 0, y'' < yMax, x'' >= 0, x'' < xMax]
        where 
            yMax = length rows
            xMax = length $ rows !! 0 

-- Checks if all cells but those containing bombs are open
hasWon :: GameField -> Bool
hasWon (GameField rows) = 
    and [((state == Closed || state == Marked) && value == Bomb) || 
            (state == Opened && value /= Bomb)  
        | row <-rows, (Cell state value) <- row] 

-- Checks if a bomb has been opened
gameOver :: GameField -> Bool
gameOver (GameField rows) = or [ state == Opened && value == Bomb 
    | row <- rows, (Cell state value) <- row]

implementation = Interface 
    {   iNewGame    = newGame
    ,   iMarkCell   = markCell
    ,   iClickCell  = clickCell
    ,   iHasWon     = hasWon
    ,   iGameOver   = gameOver
    }

start :: IO ()
start = runGame implementation