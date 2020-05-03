module MineSweeper where
import Data.Char
import Data.List.Split
import System.Random

import Control.Monad

import System.IO
import System.Console.ANSI
import Foreign.C

data State = Opened | Closed | Marked
                deriving (Eq, Show)

data Value = Numeric Int | Bomb
                deriving (Eq, Show)

data Cell = Cell { state :: State, value :: Value }
                deriving (Eq, Show)

data GameField = GameField { rows :: [[Cell]] }
                deriving (Eq, Show)

data Action = Check | Mark | Invalid 
                deriving (Eq, Show)

type Pos = (Int, Int)

data Interface = Interface 
    {   iNewGame    :: Int -> Int -> Int -> StdGen -> GameField 
    ,   iMarkCell   :: GameField -> Pos -> GameField
    ,   iCheckCell  :: GameField -> Pos -> GameField
    ,   iWinCheck   :: GameField -> Bool
    ,   iGameOver   :: GameField -> Bool
    }

runGame :: Interface -> IO ()
runGame i = do
    putStrLn "\n\n\n----------------------------------------------"
    putStrLn "MineSweeper Functional Programming Project"
    putStrLn "----------------------------------------------"
    putStrLn "\nEnter column of the game"
    x <- getLine 
    putStrLn "Enter row of the game"
    y <- getLine
    putStrLn "Enter number of bombs"
    bombs <- getLine
    putStrLn "----------------------------------------------\n"
    let column = read x :: Int
    let row = read y :: Int
    let totalBomb = read bombs :: Int
    g <- newStdGen
    loopGame i (iNewGame i column row totalBomb g) (0,0)

loopGame :: Interface -> GameField -> Pos -> IO ()
loopGame i gameField currPos = do
    printField gameField
    if (iGameOver i gameField) || (iWinCheck i gameField) then do
        finish i (iWinCheck i gameField)
    else do
        waitForKey i gameField currPos
        -- putStrLn "\nCheck [C] or Mark [M] a position, ex :  [ C row col ] or [M row col]"
        -- inputLine <- getLine
        -- let (action, pos) = changeInput inputLine
        -- if (validInput action pos gameField) then do 

        --     if action == Check then do
        --         loopGame i (iCheckCell i gameField pos)
        --     else do
        --         loopGame i (iMarkCell i gameField pos)
        -- else do
        --     putStrLn ("\n\n*************Invalid input*************\n\n")
        --     loopGame i gameField

validInput :: Action -> Pos -> GameField -> Bool
validInput Invalid _ _                = False
validInput _ (y, x) (GameField rows)  = not (y < 0 || y >= yMax || x < 0 || x >= xMax)
    where 
        yMax = length rows
        xMax = length (rows !! 0)

changeInput :: String -> (Action, Pos)
changeInput s = 
    if valid then
        if head inputs == "M" then
            (Mark, pos)
        else if head inputs == "C" then
            (Check, pos)
        else
            (Invalid, pos)
    else
        (Invalid, (-1,-1))
    where 
        inputs = splitOn " " s
        valid = length inputs == 3
        pos = (read (inputs !! 1) :: Int, read (inputs !! 2) :: Int)

getCh :: IO Char
getCh = liftM (chr . fromEnum) c_getch
foreign import ccall unsafe "conio.h _getch" c_getch :: IO CInt        

waitForKey i gameField (nowX, nowY) = do
  hSetEcho stdin False
  let rowNum = (length (rows gameField))-1
  cursorUp (1+rowNum-nowX)
  cursorForward (5+(3*nowY))
  waitForKey' (nowX,nowY)
  hSetEcho stdin True
  setSGR [Reset]
  where
    waitForKey' (x, y) = do
      key <- getCh
      case key of
        'w' -> cursorUp 1 >> waitForKey'(x-1,y)
        's' -> cursorDown 1 >> waitForKey'(x+1,y)
        'a' -> cursorBackward 3 >> waitForKey'(x,y-1)
        'd' -> cursorForward 3 >> waitForKey'(x,y+1)
        'c' -> newInput ("C " ++ show x ++ " " ++ show y)
        'm' -> newInput ("M " ++ show x ++ " " ++ show y)
        _ -> waitForKey' (x, y)

      where
        newInput inputLine = do
          let (action, pos) = changeInput inputLine
          if (validInput action pos gameField) then do 
              if action == Check then do
                  loopGame i (iCheckCell i gameField pos) (x,y)
              else do
                  loopGame i (iMarkCell i gameField pos) (x,y)
          else do
              putStrLn ("Invalid input")
              loopGame i gameField (x,y)        

finish :: Interface -> Bool -> IO ()
finish i didWin = do
    if didWin 
        then do
            setSGR [SetColor Foreground Dull Green]
            putStrLn ("\nYOU WIN !!!!!!!!")
        else do
            setSGR [SetColor Foreground Dull Red]
            putStrLn("\nYOU LOST !!!!!!!!")

printField :: GameField -> IO ()
printField (GameField rows) = do
        clearScreen
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "\n WASD - control cursor, C - Check, M - Mark\n"
        setSGR [SetColor Foreground Dull Blue]
        putStrLn ( foldl (++) "    " [showNum i ++ " " | (i, _) <- zip [0..] (rows !! 0)])   
        putStrLn ( foldl (++) "    " ["___" | _ <- (rows !! 0)])
        let row = zip [0..] rows
        mapM (\(rowNum, row) -> 
          setSGR [SetColor Foreground Dull Blue] >>
          putStr ((showNum rowNum) ++ [' ', '|', ' ']) >>
          setSGR [SetColor Foreground Dull Yellow] >>
          mapM (\c -> putStr ([cellType c] ++ "  ")) row >>
          putStrLn "") row 
        setSGR [Reset]
        -- putStrLn ( foldl (++) "    " [showNum i ++ " " | (i, _) <- zip [0..] (rows !! 0)])   
        -- putStrLn ( foldl (++) "    " ["___" | _ <- (rows !! 0)])   
        -- putStrLn (unlines [ foldl (++) ((showNum rowNum) ++ [' ', '|', ' ']) [[cellType c] ++ "  " | c <- row ] | (rowNum, row) <- zip [0..] rows] )

cellType :: Cell -> Char
cellType (Cell Closed _)              = '.'
cellType (Cell Marked _)              = 'X'
cellType (Cell Opened (Numeric 0))    = ' ' 
cellType (Cell _ (Numeric n))         = intToDigit n
cellType _                            = '@'  

showNum :: Int -> [Char]
showNum 0 = ['0', '0']
showNum x = 
    if x < 10 then 
        ['0'] ++ showNum' x
    else
        showNum' x

showNum' :: Int -> [Char]
showNum' 0 = []
showNum' x = showNum' (x `div` 10) ++ [intToDigit (x `mod` 10)]    

newGame :: Int -> Int -> Int -> StdGen -> GameField
newGame sizeX sizeY noBombs gen = addNumerics bombField bombPos
    where
        empty = emptyGameField sizeY sizeX
        bombPos = nRandPos gen noBombs [] (sizeY, sizeX)
        bombField = addBombs empty bombPos

addBombs :: GameField -> [Pos] -> GameField
addBombs gF [] = gF
addBombs (GameField rows) ((y,x):xs) = addBombs gF' xs
    where 
        gF' = GameField (rows !!= (y, rows !! y !!= (x, (Cell Closed Bomb))))

addNumerics :: GameField -> [Pos] -> GameField
addNumerics gF []                           = gF
addNumerics (GameField rows) (pos:postail)  = addNumerics gF postail
    where
        gF = addNumerics' (GameField rows) $ calcPosNbr (GameField rows) pos

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

emptyGameField :: Int -> Int -> GameField
emptyGameField maxY maxX = GameField [ row | _ <- [0..maxY]]
    where 
        row = [Cell Closed (Numeric 0) | _ <- [0..maxX]]

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


(!!=) :: [a] -> (Int,a) -> [a]
list !!= (i, v) = [ if index == i then v else value | 
    (index, value) <- zip [0..] list]


isOpened :: Cell -> Bool
isOpened (Cell Opened _)    = True
isOpened _                  = False

isMarked :: Cell -> Bool
isMarked (Cell Marked _)  = True
isMarked _                 = False

isEmptyCell :: Cell -> Bool
isEmptyCell (Cell _ (Numeric 0)) = True
isEmptyCell _                    = False 

checkCell :: GameField -> Pos -> GameField
checkCell (GameField rows) (y,x) =
    if (isOpened (Cell state v) || isMarked (Cell state v)) then 
        (GameField rows)
    else 
        if (isEmptyCell (Cell state v)) then
            checkCell' checkedGf (calcPosNbr checkedGf (y,x))
        else
            checkedGf
    where 
        checkedGf       = (GameField (rows !!= (y, rows !! y !!= (x, (Cell Opened v)))))
        (Cell state v) = rows !! y !! x

checkCell' :: GameField -> [Pos] -> GameField
checkCell' gf [pos]         = checkCell gf pos
checkCell' gf (pos:posxs)   = checkCell' gf' posxs
    where
        gf' = checkCell gf pos

setPosNbr = [(-1,-1), (-1,0), (-1,1),
            (0, -1),         ( 0,1),
            (1, -1), ( 1,0), ( 1,1)]

calcPosNbr :: GameField -> Pos -> [Pos]
calcPosNbr (GameField rows) (y,x) = 
    [(y'',x'') | (y',x') <- setPosNbr, let y'' = y+y', let x'' = x+x', y'' >= 0, y'' < yMax, x'' >= 0, x'' < xMax]
        where 
            yMax = length rows
            xMax = length $ rows !! 0 

winCheck :: GameField -> Bool
winCheck (GameField rows) = 
    and [((state == Closed || state == Marked) && value == Bomb) || 
            (state == Opened && value /= Bomb)  
        | row <-rows, (Cell state value) <- row] 

gameOver :: GameField -> Bool
gameOver (GameField rows) = or [ state == Opened && value == Bomb 
    | row <- rows, (Cell state value) <- row]

getStart = Interface 
    {   iNewGame    = newGame
    ,   iMarkCell   = markCell
    ,   iCheckCell  = checkCell
    ,   iWinCheck     = winCheck
    ,   iGameOver   = gameOver
    }

start :: IO ()
start = runGame getStart