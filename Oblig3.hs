-- Sebastian Dønnum
module Oblig3 where

import Data.Char
import Text.Read
import Data.List
import Data.Ord
import System.IO
import System.Random(getStdRandom)
import System.Random(randomR)
import Data.Maybe
import Data.Bits

{-
Nim is implemented without optimal strategy
Chomp not implemented, but gives a message when the
user tries to run it
-}

-- Player number, used to switch between human and computer player
next :: Int -> Int
next 1 = 2
next 2 = 1

-- Board is a list, where each element is number of stars
type Board = [Int]

-- Create board with desired size
initial :: Int -> Board
initial x = [1..x]

-- Check if game is finished
finished :: Board -> Bool
finished = all (== 0)

-- Checks if move is valid
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

-- Performs move on board
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board]
   where update r n = if r == row then (n - num) else n


-- Utils for board

putRow :: Int -> Int -> IO ()
putRow row  num = do putStr (show row)
                     putStr " "
                     putStrLn (concat (replicate num "* "))

--
putBoard :: Board -> Int -> IO ()
putBoard (x:xs) y = putRow y x >> putBoard xs (y + 1)

putBoard [] y = do putStr " "
                   putStrLn $ [1..y-1] >>= (' ' :) . show

-- Used to print columns of board
makeCol :: [Int] -> String
makeCol [] = ""
makeCol [x] = (show x)
makeCol (x:xs) = (show x) ++ " " ++ makeCol xs



getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'


-- Returns a tuple for the row with the most amount of pieces (fst = amount, snd = row)
aiHelper :: Board -> (Int, Int)
aiHelper xs = maximumBy (comparing fst) (zip xs [1..])

-- Gets computers row
aiRow :: Board -> IO Int
aiRow (x:xs) = do
                   let r = snd $ aiHelper (x:xs)
                   return $ r

-- Chooses number of stars that computer removes
-- No particular strategy here. If number of stars in row is
-- more than 3, it will just remove 2. Else it removes the remaining
-- stars
aiNum :: Board -> Int -> IO Int
aiNum (x:xs) r = do
                   let y = r - 1
                       n = (x:xs)!!y
                   if n > 3 then
                     return $ 2
                   else
                     return $ n



-- To run the game
play :: Board -> Int -> IO ()
play board player =
   do newline
      putBoard board 1
      if finished board then
         do newline
            if player == 2 then
              putStr "Du vant!"
            else
              putStr "Du tapte!"
            spill
      else
        if player == 1 then
           do newline
              putStr "Nim: r a / ? / q > "
              inp <- getLine
              let inputs = words inp
              case inputs of
                 ["q"] -> do
                     spill
                 ["?"] -> do
                    putStrLn "r a = fjern a brikker fra rad r"
                    putStrLn "Vinneren er den som tar siste brikke"
                    play board player

                 _ | Just [row, num] <- mapM readMaybe inputs
                    -> if valid board row num then
                          play (move board row num) (next player)
                       else
                          play board player
        else
            do newline
               putStr "Maskinen sitt trekk: "
               row <- aiRow board
               num <- aiNum board row
               if valid board row num then
                  play (move board row num) (next player)
               else
                  do newline
                     putStrLn "ERROR: Computer mistake"
                     play board player

nim :: Int -> IO ()
nim x = do
          if (x < 10) && (x > 0)  then
            play (initial x) 1
          else
            do newline
               putStr "Velg brettstørrelse 1-9!"
               spill

spill :: IO()
spill = do newline
           putStr "n(im) x / c(homp) x / q(uit) > "
           inp <- getLine
           let inputs = words inp
               h = head inputs
           -- Case block that reads input from user and perforsm
           -- desired action
           case h of
             "q" -> return()
             "n" -> do
               let size = (read(inputs !! 1) :: Int)
               nim size
             "c" -> do
               let size = (read(inputs !! 1) :: Int)
               putStrLn "Chomp er ikke implementert enda, prøv på nytt!"
               spill
