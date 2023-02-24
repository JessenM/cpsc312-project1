module TicTacToeFuncs where

import TicTacToeTypes
import DrawBoard
import Data.Char(digitToInt)

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

-- check if board has been won
getSuperBoardWinStatus :: [[WinStatus]] -> WinStatus
getSuperBoardWinStatus wslst = 
    if (checkWinStatusRows wslst  Xwin) || (checkWinStatusCols wslst Xwin) || (checkWinStatusDiags wslst Xwin)
    then Xwin
    else
        if (checkWinStatusRows wslst Owin) || (checkWinStatusCols wslst Owin) || (checkWinStatusDiags wslst Owin)
        then Owin
        else
            if checkWinStatusFull wslst
            then Draw
            else NoneYet

-- check each row of sb for three-in-a-row of type c (win condition)
checkWinStatusCols :: [[WinStatus]] -> WinStatus -> Bool
checkWinStatusCols sb c = foldl (\ v an -> v || ((sb !! 0 !! an == c) && (sb !! 1 !! an == c) && (sb !! 2!! an == c))) False [0..2]

-- check each column of sb for three-in-a-row of type c (win condition)
checkWinStatusRows :: [[WinStatus]] -> WinStatus -> Bool
checkWinStatusRows sb c = foldl (\ v an -> v || ((sb !! an !! 0 == c) && (sb !! an !! 1 == c) && (sb !! an !! 2 == c))) False [0..2]

-- check each diagonal of sb for three-in-a-row of type c (win condition)
checkWinStatusDiags :: [[WinStatus]] -> WinStatus -> Bool
checkWinStatusDiags sb c = foldl (\ v an -> v || ((sb !! (2 * an) !! 0 == c) && (sb !! 1 !! 1 == c) && (sb !! (2*(1-an)) !! (2) == c))) False [0, 1]

-- check if board is full
checkWinStatusFull :: [[WinStatus]] -> Bool
checkWinStatusFull sb = foldl (\ v an -> v && (sb !! (an `mod` 3) !! (an `div` 3)) /= NoneYet) True [0..8]


-- get subboard with given "index" (assumes index is in [1,9])
getIndexedSubBoard :: SuperBoard -> Integer -> SubBoard
getIndexedSubBoard spb index = ith whichCol (ith whichRow spb)
 where whichRow = div (index - 1) 3 + 1
       whichCol = mod (index - 1) 3 + 1
       ith 1 (h:t) = h
       ith n (h:t) = ith (n - 1) t

{-
-- get subboard with given "index" (assumes index is in [0,8])
getIndexedSubBoard :: SuperBoard -> Integer -> SubBoard
getIndexedSubBoard spb index = ith whichCol (ith whichRow spb)
 where whichRow = div index 3
       whichCol = mod index 3
       ith 0 (h:t) = h
       ith n (h:t) = ith (n - 1) t
       -}


-- insert given cell value into subboard at given coordinates (0-based indexing)
fillCell :: SubBoard -> Cell -> (Integer, Integer) -> SubBoard
fillCell board newCell (coln, rown) = fillCelln board 0
 where fillRown row n
        | row == [] = []
        | n == coln = newCell : (tail row)
        | otherwise = (head row):(fillRown (tail row) (n + 1))
       fillCelln board n
        | board == [] = []
        | n == rown = (fillRown (head board) 0):(tail board)
        | otherwise = (head board):(fillCelln (tail board) (n + 1))

-- returns list of valid actions on given superboard
-- int represents activeboardindex, if negative it means a SuperBoardAction is needed
getValidActions :: SuperBoard -> Integer -> [Action]
getValidActions superBoard activesubboardindex = 
    if activesubboardindex < 0 then
        [SuperBoardAction i | (i,subboard)<-(zip [0..] (concat superBoard)), (getSubBoardWinStatus subboard == NoneYet)]
    else
        let activesubboard = (getIndexedSubBoard superBoard (activesubboardindex+1)) in
            [SubBoardAction (rem n 3,quot n 3)| (n, cell)<-(zip [0..] (concat activesubboard)), cell==Empty]



start_state = (State startSuperBoard 4 "X")

--assumes actions are valid!!! 
ultimateTicTacToe :: Game
-- updates game with superboardaction, changes activesubboard index (superBoardActions cant end games)
-- symbols stays the same since game doesnt change turns when a superBoardAction is made
--also uses 0-indexing on both the activeSubBoardIndex and coln,rown **IF CHANGE LATER REMEMBER TO CHANGE IN FUNCTION**
ultimateTicTacToe (SuperBoardAction i) (State superBoard activeSubBoardIndex symbol) =
    ContinueGame (State superBoard i symbol)

-- updates game with subboardaction (if no one has won), if so then endgame
ultimateTicTacToe (SubBoardAction (coln,rown)) (State superBoard activeSubBoardIndex symbol)
    | currentGameStatus == Xwin = EndOfGame 1 start_state
    | currentGameStatus == Owin = EndOfGame (-1) start_state
    | currentGameStatus == Draw = EndOfGame 0 start_state
    | otherwise = ContinueGame (State updatedSuperBoard updatedActiveSubBoardIndex updatedSymbol)
     where
        updatedSuperBoard = getNewSuperBoard superBoard activeSubBoardIndex (makeNewCell(symbol)) (coln,rown)
        updatedActiveSubBoardIndex = ((3*rown)+coln)
        updatedSymbol = swapSymbol(symbol)
        currentGameStatus = processNewBoard2 updatedSuperBoard


--gets new supBoard according to activeSubBoardIndex, cell, coln, and rown.
--asbi = activesubboardindex
getNewSuperBoard :: SuperBoard -> Integer -> Cell -> (Integer, Integer) -> SuperBoard
getNewSuperBoard superBoard asbi cell (coln, rown) 
    | quot asbi 3 == 0 = [fillCellCol (superBoard!!0) asbi (coln,rown) cell, superBoard!!1, superBoard!!2] -- checks if asbi is in row 1
    | quot asbi 3 == 1 = [superBoard!!0, fillCellCol (superBoard!!1)  asbi (coln,rown) cell, superBoard!!2] -- checks if asbi is in row 2
    | otherwise = [superBoard!!0, superBoard!!1, fillCellCol (superBoard!!2) asbi (coln,rown) cell] -- checks if asbi is in row 3

--given a row of subboards and activesubboardindex, it will figure out what column the board that needs to be filled is, and fill it
fillCellCol :: [SubBoard] -> Integer -> (Integer,Integer) -> Cell -> [SubBoard]
fillCellCol subBoards asbi (coln, rown) cell
    | rem asbi 3 == 0 = [fillCell (subBoards!!0) cell (coln,rown), subBoards!!1, subBoards!!2] -- checks if asbi is in col 1, fills ifso
    | rem asbi 3 == 1 = [subBoards!!0, fillCell (subBoards!!1) cell (coln,rown), subBoards!!2] -- checks if asbi is in col 2, fills ifso
    | otherwise = [subBoards!!0, subBoards!!1, fillCell (subBoards!!2) cell (coln,rown)] -- checks if asbi is in col 3, fills ifso



--swaps Symbols
swapSymbol :: String -> String
swapSymbol "X" = "O"
swapSymbol "O" = "X"


--makes new cell
makeNewCell :: String -> Cell
makeNewCell "X" = X
makeNewCell "O" = O





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

humanPlayer :: State -> IO Action
-- player that grabs user input from human player
-- figures out if player is forced to play on current subboard, gets valid position on subboard from user
-- otherwise, gets valid superboard + subboard move from user
humanPlayer (State superBoard activesubboardindex symbol)
-- if player must play in activesubboardindex
    | let activeSubBoard = getIndexedSubBoard superBoard (activesubboardindex+1),
        getSubBoardWinStatus activeSubBoard == NoneYet = humanMakeValidSubMove superBoard (fromIntegral activesubboardindex)
--    | otherwise = humanMakeValidSubMove superBoard (fromIntegral (humanMakeValidSuperMove superBoard))
    | otherwise = humanMakeValidSuperAndSubMove superBoard

humanMakeValidSubMove :: SuperBoard -> Int -> IO Action
-- returns valid action from user (inputs a valid subboard, gets valid posiiton in subboard form user)
humanMakeValidSubMove sb index = 
    let subboard = sb !! (index `mod` 3) !! (index `div` 3) in
    do
        putStrLn ("where would you like to move on subboard " ++ show index)
        input <- getLine
        let userInput = digitToInt (input !! 0)
        if (userInput >= 0) && (userInput < 9) && (subboard !! ((userInput) `div` 3) !! (userInput `mod` 3) == Empty)
--        then return (SubBoardAction (toInteger index, toInteger userInput))
        then return (SubBoardAction (toInteger ((userInput) `mod` 3), toInteger ((userInput) `div` 3)))
        else do putStrLn "Error! not a valid move - try again" >> (humanMakeValidSubMove sb index)

{-
humanMakeValidSuperMove :: SuperBoard -> IO Integer
--returns a valid subboard from user
humanMakeValidSuperMove sb = do
    putStrLn "which superboard would you like"
    input <- getLine
    let userInput = digitToInt (input !! 0)
    if (userInput >= 0) && (userInput < 9) && (getSubBoardWinStatus (sb !! (userInput `mod` 3) !! (userInput `div` 3)) == NoneYet)
    then return (toInteger userInput)
    else humanMakeValidSuperMove sb
    -}

humanMakeValidSuperAndSubMove :: SuperBoard -> IO Action
-- returns valid action (choosing both superboard and subboard)
humanMakeValidSuperAndSubMove sb = do
    putStrLn "which superboard would you like"
    input <- getLine
    let userInput = digitToInt (input !! 0)
    if (userInput >= 0) && (userInput < 9) && (getSubBoardWinStatus (sb !! (userInput `mod` 3) !! (userInput `div` 3)) == NoneYet)
    then humanMakeValidSubMove sb userInput
    else humanMakeValidSuperAndSubMove sb

-- prints out current board
-- figures out if someone has won. if yes, announce the victory. if no, keep playing
-- using integers as return values as temporary placeholders
processNewBoard :: SuperBoard -> IO Integer
processNewBoard supboard =
    let winStat = getSubBoardWinStatus (reduceBoard supboard emptySubBoard 0) in
    do
        let boardOut = terminalDrawSuperBoard supboard
        if winStat == Xwin 
            then
                boardOut >> putStrLn "Game Over! X has won" >> return 1
            else if winStat == Owin then
                boardOut >> putStrLn "Game Over! O has won" >> return 2
            else if winStat == Draw then
                boardOut >> putStrLn "Game Over! Its a draw" >> return 3
            else boardOut >> return 4
    where
        reduceBoard:: [[SubBoard]] -> SubBoard -> Int -> SubBoard
        reduceBoard sup sub 3 = []
        reduceBoard sup sub i = (replaceRow (sup !! i) (sub !! i) 0) : (reduceBoard sup sub (i + 1))
        replaceRow :: [SubBoard] -> [Cell] -> Int -> [Cell]
        replaceRow sup row 3 = []
        replaceRow sup (row1:row2) i =
            let marker = getSubBoardWinStatus (sup !! i) in
            do
                if marker == Xwin 
                    then
                        X : (replaceRow sup row2 (i + 1))
                    else if marker == Owin then
                        O : (replaceRow sup row2 (i + 1))
                    else Empty : (replaceRow sup row2 (i + 1))

-- prints out current board
-- figures out if someone has won. if yes, announce the victory. if no, keep playing
-- using integers as return values as temporary placeholders
processNewBoard2 :: SuperBoard -> WinStatus
processNewBoard2 supboard =
    let winStat = getSuperBoardWinStatus (reduceBoard supboard emptySubBoard 0) in
    winStat
    where
        reduceBoard:: [[SubBoard]] -> SubBoard -> Int -> [[WinStatus]]
        reduceBoard sup sub 3 = []
        reduceBoard sup sub i = (replaceRow (sup !! i) (sub !! i) 0) : (reduceBoard sup sub (i + 1))
        replaceRow :: [SubBoard] -> [Cell] -> Int -> [WinStatus]
        replaceRow sup row 3 = []
        replaceRow sup (row1:row2) i =
            let marker = getSubBoardWinStatus (sup !! i) in
            do
                if marker == Xwin 
                    then
                        Xwin: (replaceRow sup row2 (i + 1))
                    else if marker == Owin then
                        Owin : (replaceRow sup row2 (i + 1))
                    else if marker == Draw then
                        Draw : (replaceRow sup row2 (i + 1))
                    else NoneYet : (replaceRow sup row2 (i + 1))


--Testing Boards

subBoard15 :: SubBoard
subBoard15 = [[X, X, X], 
            [X, O, X], 
            [O, X, Empty]]


subBoard16 = [[O, X, X], 
            [X, Empty, X], 
            [Empty, X, Empty]]

subBoard17 = [[X, X, O], 
            [O, X, X], 
            [O, O, O]]

subBoard18 = [[O, O, X], 
            [X, X, O], 
            [O, X, O]]

subBoard19 :: SubBoard
subBoard19 = [[O, O, O], 
            [X, O, X], 
            [O, X, Empty]]

            
emptysubBoard11 :: SubBoard
emptysubBoard11 = [[Empty, Empty, Empty], 
                 [Empty, Empty, Empty], 
                 [Empty, Empty, Empty]]

startSuperBoard :: SuperBoard
startSuperBoard = [[emptysubBoard11, emptysubBoard11, emptysubBoard11],
                    [emptysubBoard11, emptysubBoard11, emptysubBoard11],
                    [emptysubBoard11, emptysubBoard11, emptysubBoard11]]
superBoard11 :: SuperBoard
superBoard11 = [[emptysubBoard11, subBoard16, emptysubBoard11], 
               [subBoard17, emptysubBoard11, subBoard17], 
               [subBoard18, emptysubBoard11, subBoard18]]

superBoard12 :: SuperBoard
superBoard12 = [[emptysubBoard11, subBoard16, subBoard15], 
               [subBoard17, subBoard15, subBoard17], 
               [subBoard15, emptysubBoard11, subBoard18]]

superBoardDraw :: SuperBoard
superBoardDraw = [[subBoard18, subBoard18, subBoard18],
                  [subBoard18, subBoard18, subBoard18],
                  [subBoard18, subBoard18, subBoard18]]

superBoardNonFillDraw :: SuperBoard
superBoardNonFillDraw = [[subBoard15, subBoard19, subBoard15],
                  [subBoard19, subBoard15, subBoard15],
                  [subBoard19, subBoard15, subBoard19]]

superBoard13 :: SuperBoard
superBoard13 = [[subBoard15, subBoard16, subBoard15], 
               [subBoard17, emptysubBoard11, subBoard17], 
               [subBoard15, emptysubBoard11, subBoard18]]
               
               
testGame = ultimateTicTacToe (SubBoardAction (1,2)) start_state
testGame2 = ultimateTicTacToe (SuperBoardAction 1) start_state

-- TESTs
-- ghci> simplePlayer (State superBoard1 8 "X")
--Action at board: 1

-- State superBoard1 8 corresponds to subBoard8 in the bottom right corner, this subBoard is full, so simplePlayer computes an action choosing a new board

--ghci> simplePlayer (State superBoard1 4 "X")
--Action at: col: 1 row: 1

-- State superBoard1 4 corresponds to emptySubBoard in the middle, this subBoard is empty, so simplePlayer computes an action choosing a cell on that board
