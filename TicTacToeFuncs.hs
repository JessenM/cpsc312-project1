module TicTacToeFuncs where

import TicTacToeTypes

checkRows :: SubBoard -> Cell -> Int -> Bool
checkRows _ _ 4 = False
checkRows subboard symbol index = 
    if (subboard !! index !! 1 == symbol) && (subboard !! index !! 2 == symbol) && (subboard !! index !! 3 == symbol)
    then True
    else checkRows subboard symbol (index + 1)

checkCols :: SubBoard -> Cell -> Int -> Bool
checkCols _ _ 4 = False
checkCols subboard symbol index = 
    if (subboard !! 1 !! index == symbol) && (subboard !! 2 !! index == symbol) && (subboard !! 3 !! index == symbol)
    then True
    else checkCols subboard symbol (index + 1)

checkDiags :: SubBoard -> Cell -> Int -> Bool
checkDiags _ _ 3 = False
checkDiags subboard symbol index = 
    if (subboard !! (2 * index - 1) !! 1 == symbol) && (subboard !! 2 !! 2 == symbol) && (subboard !! (5 - 2 * index) !! 3 == symbol)
    then True
    else checkDiags subboard symbol (index + 1)

checkFull :: SubBoard -> Bool
checkFull [[]] =  True
checkFull a =  True -- PLACEHOLDER
--checkFull ((sb1:sb2):sb3) = if sb1 == Empty then False else checkFull (sb2:[]):sb3

getSubBoardWinStatus :: SubBoard -> WinStatus
getSubBoardWinStatus sb = 
    if (checkRows sb X 1) || (checkCols sb X 1) || (checkDiags sb X 1)
    then Xwin
    else
        if (checkRows sb O 1) || (checkCols sb O 1) || (checkDiags sb O 1)
        then Owin
        else
            if checkFull sb
            then Draw
            else NoneYet