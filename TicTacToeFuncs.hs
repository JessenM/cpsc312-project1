module TicTacToeFuncs where

import TicTacToeTypes

checkRows :: SubBoard -> Cell -> Bool
checkRows sb c = foldl (\ v an -> v || ((sb !! 1 !! an == c) && (sb !! 2 !! an == c) && (sb !! 3 !! an == c))) False [1..3]
--checkRows :: SubBoard -> Cell -> Int -> Bool
--checkRows _ _ 4 = False
--checkRows subboard symbol index = 
--    if (subboard !! index !! 1 == symbol) && (subboard !! index !! 2 == symbol) && (subboard !! index !! 3 == symbol)
--    then True
--    else checkRows subboard symbol (index + 1)

checkCols :: SubBoard -> Cell -> Bool
checkCols sb c = foldl (\ v an -> v || ((sb !! an !! 1 == c) && (sb !! an !! 2 == c) && (sb !! an !! 3 == c))) False [1..3]
--checkCols :: SubBoard -> Cell -> Int -> Bool
--checkCols _ _ 4 = False
--checkCols subboard symbol index = 
--    if (subboard !! 1 !! index == symbol) && (subboard !! 2 !! index == symbol) && (subboard !! 3 !! index == symbol)
--    then True
--    else checkCols subboard symbol (index + 1)

checkDiags :: SubBoard -> Cell -> Bool
checkDiags sb c = foldl (\ v an -> v || ((sb !! (2 * an - 1) !! 1 == c) && (sb !! 2 !! 2 == c) && (sb !! (5 - 2 * an) !! 3 == c))) False [1, 2]
--checkDiags :: SubBoard -> Cell -> Int -> Bool
--checkDiags _ _ 3 = False
--checkDiags subboard symbol index = 
--    if (subboard !! (2 * index - 1) !! 1 == symbol) && (subboard !! 2 !! 2 == symbol) && (subboard !! (5 - 2 * index) !! 3 == symbol)
--    then True
--    else checkDiags subboard symbol (index + 1)

checkFull :: SubBoard -> Bool
-- checkFull [[]] =  True
checkFull sb = foldl (\ v an -> v && (sb !! (an `mod` 3 + 1) !! (an `div` 3 + 1)) == Empty) True [0..8]

getSubBoardWinStatus :: SubBoard -> WinStatus
getSubBoardWinStatus sb = 
    if (checkRows sb X) || (checkCols sb X) || (checkDiags sb X)
    then Xwin
    else
        if (checkRows sb O) || (checkCols sb O) || (checkDiags sb O)
        then Owin
        else
            if checkFull sb
            then Draw
            else NoneYet