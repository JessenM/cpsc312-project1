module TicTacToeFuncs where

import TicTacToeTypes

-- check each row of sb for three-in-a-row of type c (win condition)
checkCols :: SubBoard -> Cell -> Bool
checkCols sb c = foldl (\ v an -> v || ((sb !! 0 !! an == c) && (sb !! 1 !! an == c) && (sb !! 2!! an == c))) False [0..2]

-- check each column of sb for three-in-a-row of type c (win condition)
checkRows :: SubBoard -> Cell -> Bool
checkRows sb c = foldl (\ v an -> v || ((sb !! an !! 0 == c) && (sb !! an !! 1 == c) && (sb !! an !! 2 == c))) False [0..2]

-- check each diagonal of sb for three-in-a-row of type c (win condition)
checkDiags :: SubBoard -> Cell -> Bool
checkDiags sb c = foldl (\ v an -> v || ((sb !! (2 * an) !! 0 == c) && (sb !! 1 !! 1 == c) && (sb !! (2*(1-an)) !! (2) == c))) False [0, 1]

-- check if board is full
checkFull :: SubBoard -> Bool
checkFull sb = foldl (\ v an -> v && (sb !! (an `mod` 3) !! (an `div` 3)) /= Empty) True [0..8]

-- check if board has been won
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