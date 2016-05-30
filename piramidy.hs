{-# LANGUAGE FlexibleContexts #-}
{- This is a puzzle sovler for a puzzle named Piramidy.
   The input data for the puzzle is read from a .txt file defined in main function
        Sample input data:
        Piramidy [Just 3, Nothing, Just 1, Nothing] [Nothing, Nothing, Nothing,Nothing] [Nothing, Nothing, Just 4, Nothing] [Nothing, Just 3, Nothing, Nothing]

        This translates to:	
            3 1   
          "-------"
          |2 1 4 3| 
          |3 4 2 1| 3
         4|1 2 3 4|
          |4 3 1 2|
          "-------"

-}
import System.Environment
import Data.Maybe
import Data.List
import Control.Monad.State
import System.IO
import Data.Array.IO
import Debug.Trace
import System.Directory

data Piramidy = Piramidy [Maybe Int] [Maybe Int] [Maybe Int] [Maybe Int] deriving (Show, Read)
type Board = IOArray Int Int --array representing the values of all cells of the board (Indexing from starts at 1, not 0)

main = do
    putStrLn "Enter the name of the file containg edge constraits for the puzzle"
    filename <- getLine
    fileOK <- doesFileExist filename
    if (fileOK)
        then do
            contents <- readFile filename
            let piramidy@(Piramidy above below left right) = read contents :: Piramidy
                n = length above
            a <- newArray (1, n*n) 0 :: IO (Board)
            printSolution piramidy n a
            putStrLn "\nsolution:\n"
            solve piramidy n a (1,1)
            getLine
        else do 
            putStrLn ("file does not exist!!")
            getLine
 
-- prints the whole board along with the found solution
printSolution (Piramidy above below left right) n a  =
   let getC Nothing = " "
       getC (Just c) = show c
       printLine n a row  = putStr ( getC (left !! (row-1))) >> putStr "|" 
                                     >> mapM (\x -> readBoard a (x,row) n) [1..n] >>= mapM_ (putStr . show) 
                                     >> putStr "|" >> putStrLn (getC (right !! (row-1)))      
   in do putStr  "  " >> mapM_ (\c -> putStr $ getC c ) above >> putStrLn " "
         putStrLn  (" "  ++ show (concat ["-" | _ <- [1..n]])) 
         mapM_ (\y -> printLine n a y) [1..n]
         putStrLn  (" "  ++ show (concat ["-" | _ <- [1..n]]))
         putStr  "  " >> mapM_ (\c -> putStr $ getC c ) below >> putStrLn " "
 
{- main function solving the puzzle: 
    goes through each cell in a row starting from the first column, then continues to the start of the next row and so on
    returns a filled out Board if one was found -}
solve :: Piramidy -> Int -> Board ->(Int, Int) -> IO (Maybe Board)
solve p n a (col, row) | col == n+1 = do 
                            piramidsFailed <- edgeConstraintsViolated p n a (col-1,row) -- go to next row              
                            if (piramidsFailed)
                               then return Nothing
                               else solve p n a (1, row+1) 
                                     
                       | row == n+1 = printSolution p n a  >> return (Just a) -- reached the last cell (n,n) of the board in the previous iteration
                       | otherwise = do 
                            if (row == n && col > 1) -- reached the last row and column other than 1
                                then do
                                    piramidsFailed <- edgeConstraintsViolated p n a (col-1,row) --check if tcell values are violating edge constraints
                                    if (piramidsFailed)
                                        then return Nothing
                                        else findVal
                                else findVal
                                
                            -- solve' handles the backtacking
                            where findVal = do v <- readBoard a (col,row) n
                                               case v of
                                                    0 -> availableNums p n a (col,row) >>= solve' p n a (col,row) -- nothing in this cell yet
                                                    _ ->  solve p n a (col+1,row) --a value is already there, continue to the next cell
                            
                                  solve' p n a (col,row) []     = return Nothing
                                  solve' p n a (col,row) (v:vs) = do 
                                        writeBoard a (col,row) n v -- put a guess onto the board
                                        r <- solve p n a (col+1,row)
                                        if (r == Nothing )
                                            then do   --backtrack
                                              writeBoard a (col,row) n 0  -- remove the guess from the board
                                              solve' p n a (col,row) vs   -- recurse over the remainder of the list
                                            else return r
                                                                
edgeConstraintsViolated (Piramidy above below left right) n a (0,row) = do return False -- safe check against array out of bounds exception
edgeConstraintsViolated (Piramidy above below left right) n a (col,row) = do 
    r <- getRowVals a row n
    c <- getColVals a col n
    --find all edge constraints for this cell   
    let cAbove = above !! (col-1) 
        cBelow = below !! (col-1)
        cLeft  = left  !! (row-1)
        cRight = right !! (row-1) 
        fail = not ((isLineOK cAbove c) && (isLineOK cBelow (reverse c)) 
                     && (isLineOK cLeft r) && (isLineOK cRight (reverse r)))  
    return (fail)
 
isLineOK (Just constraint) line = (emptyCells line == 0 && numVisiblePiramids line == constraint) || emptyCells line > 0
isLineOK Nothing _ = True

--calculate how many zeros left in a line
emptyCells line = length ([val | val <- line, val == 0]) 

--check how many Piramids is visible in a given line
numVisiblePiramids [] = 0
numVisiblePiramids (x:xs) | x > 0 = 1 + numVisiblePiramids (map (\y -> y-x) xs)
                          | otherwise = numVisiblePiramids xs 
                                                                                                                    
-- return the numbers that are available for a particular cell
availableNums p n a (col,row) = do   r <- getRowVals a row n
                                     c <- getColVals a col n
                                     h <- getAllForbiddenHeights p n (col,row)                                      
                                     let a = [0..n] \\ (r `union` c `union` h)
                                     return a
    
    
-- get the unavailable numbers from a row, col and depending on height constraints.
getRowVals a row n = sequence [readBoard a (col',row) n | col' <- [1..n]]
getColVals a col n = sequence [readBoard a (col,row') n | row' <- [1..n]]
getAllForbiddenHeights (Piramidy above below left right) n (col,row) =  do 
    return  (forbiddenHeightsForLine (above !! (col-1)) n row
          ++ forbiddenHeightsForLine (below !! (col-1)) n (n-row+1)
          ++ forbiddenHeightsForLine (left !! (row-1)) n col
          ++ forbiddenHeightsForLine (right !! (row-1)) n (n-col+1))

--return a list of values forbidden by edge constraints depending on cell position                                                                               --
forbiddenHeightsForLine Nothing _ _ = []
forbiddenHeightsForLine (Just 1) n index | index==1 = [1..n] \\ [n]
                                         | otherwise = [n]
                                         
forbiddenHeightsForLine (Just constraint) n index | constraint == n = ([1..n] \\ [index])
                                                  | otherwise = [1..n] \\ [1..index+n-constraint]
    
--The board has cells numbered form (1,1) to (n,n)    
readBoard a (col,row) n = readArray a (col+n*(row-1))
writeBoard a (col,row) n v = writeArray a (col+n*(row-1)) v

--a helper function for debugging
traceThis msg x = trace (msg ++ show x) x 
                                            


























