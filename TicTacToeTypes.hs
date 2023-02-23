module TicTacToeTypes where


--(Cell, SubBoard, SuperBoard, Game, Player, Action, Result, State) where

-- a cell is either an X, an O, or empty (not filled yet)
data Cell = X
            | O
            | Empty
        deriving (Eq, Show)

-- a subboard is just a list of lists (rows) of cells
--ex.
-- subBoard1 = [[X    , Empty,   O  ]      --row1
--               [O    , Empty, Emtpy]     --row2
--               [Empty, X    ,   O   ]]   --row3

type SubBoard = [[Cell]]

emptySubBoard :: SubBoard
emptySubBoard = [[Empty, Empty, Empty], 
                 [Empty, Empty, Empty], 
                 [Empty, Empty, Empty]]

-- superboard is just a list of lists (rows) of subboards (similar to above)
type SuperBoard = [[SubBoard]]

emptySuperBoard :: SuperBoard
emptySuperBoard = [[emptySubBoard, emptySubBoard, emptySubBoard], 
                   [emptySubBoard, emptySubBoard, emptySubBoard], 
                   [emptySubBoard, emptySubBoard, emptySubBoard]]

-- Game takes a action (coordinate specifiying a move), a state (board), and returns a result
type Game = Action -> State -> Result

-- Player takes a state (board) and returns an action (coordinate specifiying move)
type Player = State -> Action


-- an action is either:
-- SubBoardAction - coordinate on subboard to make move, (ColumnNumber, RowNumber), INDEX ([0..2],[0..2])
-- SuperBoardAction - choosing a subboard from the superboard (one of the 9), INDEX [0..8]
data Action = SubBoardAction (Integer, Integer)
              |SuperBoardAction Integer

instance Show Action where
	show (SubBoardAction (coln, rown)) = "Action at: col: "++(show(coln+1))++" row: "++(show(rown+1)) -- use +1 because index starts at 0, but our GUI coordinates start at 1
	show (SuperBoardAction boardnumber) = "Action at board: "++(show (boardnumber+1)) -- +1 because index starts at 0

-- Result is either EndOfGame with double encoding outcome, or ContinueGame with an updated state 
data Result = EndOfGame Double State
            |ContinueGame State
        deriving (Show, Eq)
        
-- the State is:
-- The Superboard (contains the subboards, the available (empty) cells, and cells that have been filled (X, O))
-- Integer that signifies which subBoard is playing played upon (1-9)
-- String that signifies which symbol is currently playing ('X' or 'O')
data State = State SuperBoard Integer String
        deriving (Eq, Show)

-- a WinStatus is either an X, an O, a draw (all squares filled in, no winner), NoneYet
data WinStatus = Xwin
            | Owin
            | Draw
            | NoneYet
        deriving (Eq, Show)