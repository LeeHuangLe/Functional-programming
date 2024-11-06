import Bin
import Cmd
import Parser

import Bin (Bin, BinZip, Tile(..), go_left, go_right, go_down, drawBinZip)
import Parser (parseInput)
import System.IO (hFlush, stdout)
import GhostMovements (updateTree,Direction(..))

import System.IO
import Control.Concurrent (threadDelay) 
-- a small binary tree
complexTree :: Bin
complexTree = N 1 Pacman 
               (N 2 Path 
                 (L 3 Pellet) 
                 (N 4  Ghost 
                   (L 5 Wall) 
                   (L 6 EmptyTile)))
               (N 7 Ghost 
                 (N 8 Path 
                   (L 9 Pellet) 
                   (L 1 Wall)) 
                 (L 11 EmptyTile))

-- the top-level interactive loop
go :: BinZip -> Int -> Int -> IO ()
go z score lives = do
                    displayCurrentPosition z
                    putStr "> "
                    hFlush stdout
                    line <- getLine
        
                    case parseInput parseCmd line of
                      Nothing -> do
                        putStrLn "I'm sorry, I do not understand."
                        go z score lives
                      (Just Go_Left) -> tryMove go_left "left" z score lives
                      (Just Go_Right) -> tryMove go_right "right" z score lives
                      (Just Go_Down) -> tryMove go_down "down" z score lives
                      (Just Show )   -> do 
                                        putStrLn (drawBinZip z)
                                        go z score lives
                      (Just Quit) -> do
                        putStrLn "Okay."
                        putStrLn "You ended the game over here:\n"
                        putStrLn (drawBinZip z)
                        putStrLn $ "Final Score: " ++ show score
                        putStrLn "Goodbye."

    -- Display current tile and handle interaction
displayCurrentPosition :: BinZip -> IO ()
displayCurrentPosition (_, t) = handleTile t

handleTile :: Bin -> IO ()
handleTile (L _ Pacman) = putStrLn "You are Pacman."
handleTile (L _ Pellet) = putStrLn "You see a Pellet."
handleTile (L _ Wall)   = putStrLn "You see a Wall. You cannot move here."
handleTile (L _ EmptyTile) = putStrLn "You see an empty space."
handleTile (L _ Ghost)  = putStrLn "You see a Ghost. Watch out!"
handleTile (L _ Path)  = putStrLn "You see a path!"
handleTile (N _ Pacman _ _) = putStrLn "You are Pacman."
handleTile (N _ Pellet _ _) = putStrLn "You see a Pellet."
handleTile (N _ Wall _ _)   = putStrLn "You see a Wall. You cannot move here."
handleTile (N _ EmptyTile _ _) = putStrLn "You see an empty space."
handleTile (N _ Ghost _ _)  = putStrLn "You see a Ghost. Watch out!"
handleTile (N _ Path _ _)  = putStrLn "You see a Path"





-- Try moving in the specified direction, handling game rules
tryMove :: (Maybe BinZip -> Maybe BinZip) -> String -> BinZip -> Int -> Int -> IO ()
tryMove move dir z score lives = do
                                  case ((updateTree (move (Just z)) True LLeft)) of
                                    Just newZ -> case snd newZ of
                                                    L _ Wall -> do
                                                      putStrLn $ "You cannot move " ++ dir ++ ", there's a Wall."
                                                      go z score lives
                                                    L _ Pellet -> do
                                                      putStrLn "Pacman eats a Pellet!"
                                                      go newZ (score + 10) lives
                                                    L _ Ghost -> do
                                                      putStrLn "Pacman encounters a Ghost!"
                                                      if lives > 1
                                                        then do
                                                          putStrLn "You lost a life!"
                                                          go (Hole, complexTree) score (lives - 1) -- Reset position
                                                        else do
                                                          putStrLn "Game Over!"
                                                          putStrLn $ "Final Score: " ++ show score
                                                    _ -> go newZ score lives
                                    Nothing -> do
                                      putStrLn $ "You cannot move " ++ dir ++ " any further."
                                      go z score lives
repl :: IO ()
repl = do
        putStrLn "Welcome to Pacman.\n"
        putStrLn "You are at the root of a binary tree."
        go (Hole, complexTree) 0 3  -- Start with score 0 and 3 lives

         
main = repl
