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


-- get subboard with given "index" (assumes index is in [1,9])
getIndexedSubBoard :: SuperBoard -> Integer -> SubBoard
getIndexedSubBoard spb index = ith whichCol (ith whichRow spb)
 where whichRow = div (index - 1) 3 + 1
       whichCol = mod (index - 1) 3 + 1
       ith 1 (h:t) = h
       ith n (h:t) = ith (n - 1) t

-- returns list of valid actions on given superboard
-- int represents activeboardindex, if negative it means a SuperBoardAction is needed
getValidActions :: SuperBoard -> Integer -> [Action]
getValidActions superBoard activesubboardindex = 
    if activesubboardindex < 0 then
        [SuperBoardAction i | (i,subboard)<-(zip [0..] (concat superBoard)), (getSubBoardWinStatus subboard == NoneYet)]
    else
        let activesubboard = (getIndexedSubBoard superBoard (activesubboardindex+1)) in
            [SubBoardAction (rem n 3,quot n 3)| (n, cell)<-(zip [0..] (concat activesubboard)), cell==Empty]


simplePlayer :: Player
-- very simple player
-- if choosing subboard, it chooses the first board
-- if choosing a cell on subboard, it chooses the first cell
-- if activeSubBoard is full or won/draw then computes a superBoard action
simplePlayer (State superBoard activesubboardindex symbol) 
    | let activeSubBoard = getIndexedSubBoard superBoard (activesubboardindex+1), not (checkFull(activeSubBoard)) && ((getSubBoardWinStatus activeSubBoard) == NoneYet) = head(getValidActions superBoard activesubboardindex)
    | otherwise = head(getValidActions superBoard (-1))

-- first guard checks to see if active subboard is full or won, if not then computes an action on that subboard
-- otherwise, computes an action that chooses a new subboard


-- current implementation does not use a negative activesubboardindex, checks to see if full or won

--Testing Boards

subBoard5 :: SubBoard
subBoard5 = [[X, X, X], 
            [X, O, X], 
            [O, X, Empty]]


subBoard6 = [[O, X, X], 
            [X, Empty, X], 
            [Empty, X, Empty]]

subBoard7 = [[X, X, O], 
            [O, X, X], 
            [O, O, O]]

subBoard8 = [[O, O, X], 
            [X, X, O], 
            [O, X, O]]

emptySubBoard1 :: SubBoard
emptySubBoard1 = [[Empty, Empty, Empty], 
                 [Empty, Empty, Empty], 
                 [Empty, Empty, Empty]]

superBoard1 :: SuperBoard
superBoard1 = [[emptySubBoard1, subBoard6, emptySubBoard1], 
                [subBoard7, emptySubBoard, subBoard7], 
                [subBoard8, emptySubBoard1, subBoard8]]

-- TESTs
-- ghci> simplePlayer (State superBoard1 8 "X")
--Action at board: 1

-- State superBoard1 8 corresponds to subBoard8 in the bottom right corner, this subBoard is full, so simplePlayer computes an action choosing a new board

--ghci> simplePlayer (State superBoard1 4 "X")
--Action at: col: 1 row: 1

-- State superBoard1 4 corresponds to emptySubBoard in the middle, this subBoard is empty, so simplePlayer computes an action choosing a cell on that board
