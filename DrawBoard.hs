--DrawBoard
module DrawBoard where

import TicTacToeTypes
import Data.List

--Functions for showing Board data in terminal

-- converts all of its subboards into string representations, then combines them together
--drawSuperBoard :: SuperBoard -> String

subBoard2 :: SubBoard
subBoard2 = [[X, Empty, O], 
            [Empty, X, Empty], 
            [X, Empty, O]]
            
subBoard3 :: SubBoard        
subBoard3 = [[O, X, O], 
            [Empty, O, X], 
            [Empty, X, X]]
            
subBoard4 :: SubBoard        
subBoard4 = [[O, Empty, Empty], 
            [O, X, Empty], 
            [Empty, Empty, X]]

superBoard2 :: SuperBoard
superBoard2 = [[subBoard2, subBoard2, subBoard2], [subBoard2, subBoard2, subBoard2], [subBoard2, subBoard2, subBoard2]]

superBoard3 :: SuperBoard
superBoard3 = [[subBoard2, subBoard3, subBoard4],[subBoard4, subBoard2, subBoard3],[subBoard3, subBoard4, subBoard2]]
            
emptySubBoard1 = [[Empty, Empty, Empty], 
                 [Empty, Empty, Empty], 
                 [Empty, Empty, Empty]]
            
            
--converts single subboard into string representation (ie show instance for subboard)
drawSubBoard :: SubBoard -> [Char]
drawSubBoard b =
    boardTop++"\n"++(intercalate betweenRow (boardSide (map makeStrRows b)))
    where
        boardSize = length (head b)--we know the subBoards are always 3x3, but this makes sure
        boardTop = "  "++(intercalate  "   " (strToChars (take boardSize ['1'..]))) -- top part of the board, with column labels 1 2 3
        betweenRow = "\n"++" ---+---+---"++"\n" -- what appears between the rows
        makeStrRows = intercalate " | " . map cellToChar --converts row of cells to row of chars with seperation
        boardSide str = [(show n)++" "++row| n <- [0..(boardSize-1)], row<-[str!!n]] -- takes a str row and adds index to front of it


drawSuperBoard :: SuperBoard -> [Char]
drawSuperBoard b = 
    boardTop++"\n \n"++(constructSuperBoard (makeListRowSB (superBoardToStringSuperBoard b)))
    where
        boardSize = length (head b)
        boardTop = "    "++(intercalate  "            " (strToChars (take boardSize ['A'..])))


-- Test functions to test what the draw functions look like in terminal
terminalDrawSubBoard :: SubBoard -> IO ()
terminalDrawSubBoard b = putStrLn (drawSubBoard b)

terminalDrawSuperBoard :: SuperBoard -> IO ()
terminalDrawSuperBoard b = putStrLn (drawSuperBoard b)


--HELPER Functions

--DISCLAIMER:
-- a superboard-row is a row of subboards that appear on a superbpard
-- a subboard-row is the corresponding 3-rows that occur in consecutive subBoards, there are 9 in total for a superBoard
-- a superboard-row is created from 3, corresponding subboard-rows

--creates a lists of strings corresponding to list of subboards
makeStrSuperBoards :: [SubBoard] -> [[[Char]]]
makeStrSuperBoards = map (map (intercalate " | " . map cellToChar))

-- creates a string superboard ([[stringsubboards]])
superBoardToStringSuperBoard :: SuperBoard -> [[[[Char]]]]
superBoardToStringSuperBoard [] = []
superBoardToStringSuperBoard (h:t) = (makeStrSuperBoards(h)):superBoardToStringSuperBoard(t)

-- make list of subboard row entries corresponding to n and m, ready to be intercalated with separators
makeListSubBoardRowSB :: [[[[Char]]]] -> Int -> Int -> [[Char]]
makeListSubBoardRowSB strSB n m = [((strSB!!(n-1))!!0)!!(m-1), ((strSB!!(n-1))!!1)!!(m-1), ((strSB!!(n-1))!!2)!!(m-1)]

-- creates a single, string representing 1 subboard-row 
makeStringSubBoardRowSB :: [[Char]] -> [Char]
makeStringSubBoardRowSB sbrow = intercalate " || " sbrow

-- makes list of subboard-rows of superBoard
makeListSubBoardSB :: [[[[Char]]]] -> Int -> [[Char]]
makeListSubBoardSB strSB n = [(makeStringSubBoardRowSB (makeListSubBoardRowSB strSB n 1)), makeStringSubBoardRowSB (makeListSubBoardRowSB strSB n 2), makeStringSubBoardRowSB (makeListSubBoardRowSB strSB n 3)]

--puts seperators of ---- between each subboard-row
makeStringSubBoardSB :: [[Char]] -> [Char]
makeStringSubBoardSB lststr = intercalate "\n--+---+-- || --+---+-- || --+---+-- \n" lststr

-- makes list of superboard-rows ready to be intercalated with sepators (also labelled)
makeListRowSB :: [[[[Char]]]] -> [[Char]]
makeListRowSB strSB = [(makeStringSubBoardSB (makeListSubBoardSB strSB 1))++"   A", (makeStringSubBoardSB (makeListSubBoardSB strSB 2))++"   B", (makeStringSubBoardSB (makeListSubBoardSB strSB 3))++"   C"]

--Puts seperators of === between each superboard-row on superboard
constructSuperBoard :: [[Char]] -> [Char]
constructSuperBoard lststr = intercalate "\n==========++===========++========== \n" lststr


testdrawing = 
    do
        terminalDrawSubBoard subBoard2
        putStrLn "\n"
        terminalDrawSuperBoard superBoard3


--Takes a string and splits into into a list of each of its characters
strToChars :: [Char] -> [[Char]]
strToChars [] = []
strToChars (h:t) = [h]:(strToChars t)

--Takes a list of cells and splits into list of characters
cellToChar :: Cell -> [Char]
cellToChar c = (changeifEmpty(show c))

-- Changes 'Empty' to blank for better board GUI
changeifEmpty :: [Char] -> [Char]
changeifEmpty "Empty" = " "
changeifEmpty str = str