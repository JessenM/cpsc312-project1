-- CPSC 312 - 2023 - Games in Haskell
-- adapted by Amanda Tupper for Ultimate Tic-Tac-Toe
-- (reason for adaptation: desire for consecutive meaningful inputs from a player)
module Play where

-- To run it, try:
-- ghci
-- :load Play
-- play ultimateTicTacToe start_state simplePlayer (0,0,0)

import TicTacToeTypes
import TicTacToeFuncs
import DrawBoard

import System.IO
import Text.Read   (readMaybe)

type TournammentState = (Int,Int,Int)   -- wins, losses, ties


play :: Game -> State -> Player -> TournammentState -> IO TournammentState

play game start_state opponent ts =
  let (wins, losses,ties) = ts in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if line == "0"
        then
            person_play game (ContinueGame start_state) opponent ts
        else if line ==  "1"
             then computer_play game (ContinueGame start_state) opponent ts
        else if line == "2"
            then return ts
        else play game start_state opponent ts

person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play

-- TODO requires adaptation
--      multiple-input (main goal)
person_play game (ContinueGame state) opponent ts =
   do
      let State board _ _ = state
      putStrLn ("Board: \n"++drawSuperBoard board)
      move <- humanPlayer state
      computer_play game (game move state) opponent ts

person_play game (EndOfGame val start_state) opponent ts =
  do
    newts <- update_tournament_state (-val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent newts

computer_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent ts
-- person has played, the computer must now play
computer_play game (EndOfGame val  start_state) opponent ts =
   do
      newts <- update_tournament_state val ts
      play game start_state opponent newts

computer_play game (ContinueGame state) opponent ts =
      let 
          opponent_move = opponent state
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game opponent_move state) opponent ts

update_tournament_state:: Double -> TournammentState -> IO TournammentState
-- given value to the person, the tournament state, return the new tournament state
update_tournament_state val (wins,losses,ties)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses,ties)
  | val == 0 = do
      putStrLn "It's a tie"
      return (wins,losses,ties+1)
  | otherwise = do
      putStrLn "Computer won!"
      return (wins,losses+1,ties)
