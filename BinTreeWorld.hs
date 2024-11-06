import Bin
import Cmd
import Parser

import Bin 
import MoveFunctions
import Parser (parseInput)
import System.IO (hFlush, stdout)
import GhostMovements (updateTree)

import System.IO
import Control.Concurrent (threadDelay) 
-- a small binary tree
complexTree :: Bin
complexTree = N 1 EmptyTile 
               (N 2 EmptyTile 
                 (N 3 EmptyTile 
                   (N 4 Pellet 
                     (L 8 EmptyTile) 
                     (L 9 Pellet)) 
                   (L 5 EmptyTile)) 
                 (N 6 Ghost 
                   (N 10 EmptyTile 
                     (L 12 Pellet) 
                     (L 21 EmptyTile)) 
                   (L 11 EmptyTile)))
               (N 7 Ghost 
                 (N 14 EmptyTile 
                   (N 15 EmptyTile 
                     (L 2 EmptyTile) 
                     (L 17 Pellet)) 
                   (L 18 EmptyTile)) 
                 (N 19 EmptyTile 
                   (L 20 Pellet) 
                   (N 21 EmptyTile 
                     (L 5 EmptyTile) 
                     (L 23 Pellet))))

number :: Int
number = 6

-- the top-level interactive loop
go :: BinZip -> Int -> Int -> Int ->Int -> IO ()
go z score lives seed n = do
                    displayCurrentPosition z
                    if n > 0 then do
                                    putStr "> "
                                    hFlush stdout
                                    line <- getLine                   
                                    case parseInput parseCmd line of
                                            Nothing -> do
                                                        putStrLn "I'm sorry, I do not understand."
                                                        go z score lives (seed) n
                                                        putStrLn $ show n
                                            (Just Wait)     -> tryMove wait "wait" z score lives (seed+1) n
                                            (Just Teleport) -> tryMove teleport "teleport" z score lives (seed+1) n
                                            (Just Go_Left)  -> tryMove go_left "left" z score lives (seed+1) n 
                                            (Just Go_Right) -> tryMove go_right "right" z score lives (seed+1) n
                                            (Just Go_Down)  -> tryMove go_down "down" z score lives (seed+1) n
                                            (Just Show)     -> do 
                                                                putStrLn (drawBinZip z)
                                                                putStrLn ("The number is: " ++ show n)
                                                                go z score lives (seed+1) n
                                                                
                                            (Just Quit)     -> do
                                                                putStrLn "Okay."
                                                                putStrLn "You ended the game over here:\n"
                                                                putStrLn (drawBinZip z)
                                                                putStrLn $ "Final Score: " ++ show score
                                                                putStrLn "Goodbye."
                                else putStrLn "Congratulations, You collected all pellets and you won."

    -- Display current tile and handle interaction
displayCurrentPosition :: BinZip -> IO ()
displayCurrentPosition (_, t) = handleTile t

handleTile :: Bin -> IO ()
handleTile (L _ Pellet) = putStrLn "You see a Pellet."
handleTile (L _ Wall)   = putStrLn "You see a Wall. You cannot move here."
handleTile (L _ EmptyTile) = putStrLn "You see an empty space."
handleTile (L _ Ghost)  = putStrLn "You see a Ghost. Watch out!"
handleTile (N _ Pellet _ _) = putStrLn "You see a Pellet."
handleTile (N _ Wall _ _)   = putStrLn "You see a Wall. You cannot move here."
handleTile (N _ EmptyTile _ _) = putStrLn "You see an empty space."
handleTile (N _ Ghost _ _)  = putStrLn "You see a Ghost. Watch out!"

-- Try moving in the specified direction, handling game rules
tryMove :: (Maybe BinZip -> Maybe BinZip) -> String -> BinZip -> Int -> Int -> Int -> Int -> IO ()
tryMove move dir z score lives seed n = do
                                  case ((updateTree (move (Just z)) True LLeft (seed + 1))) of
                                    Just newZ -> case snd newZ of
                                                    L _ Wall -> do
                                                      putStrLn $ "You cannot move " ++ dir ++ ", there's a Wall."
                                                      go z score lives seed n
                                                    L _ Pellet -> do
                                                      putStrLn "Pacman eats a Pellet!"
                                                      go (removeTile newZ) (score + 10) lives seed (n - 1)
                                                    L _ Ghost -> do
                                                      putStrLn "Pacman encounters a Ghost!"
                                                      if lives > 1
                                                        then do
                                                          putStrLn "You lost a life!"
                                                          go (Hole, complexTree) score (lives - 1) seed number -- Reset position
                                                        else do
                                                          putStrLn "Game Over!"
                                                          putStrLn $ "Final Score: " ++ show score
                                                    L _ Just_moved_Ghost -> do
                                                      putStrLn "Pacman encounters a Ghost!"
                                                      if lives > 1
                                                        then do
                                                          putStrLn "You lost a life!"
                                                          go (Hole, complexTree) score (lives - 1) seed number-- Reset position
                                                        else do
                                                          putStrLn "Game Over!"
                                                          putStrLn $ "Final Score: " ++ show score
                                                    N _ Wall _ _ -> do
                                                      putStrLn $ "You cannot move " ++ dir ++ ", there's a Wall."
                                                      go z score lives seed n
                                                    N _ Pellet _ _ -> do
                                                      putStrLn "Pacman eats a Pellet!"
                                                      go (removeTile newZ) (score + 10) lives seed (n - 1)
                                                    N _ Ghost _ _ -> do
                                                      putStrLn "Pacman encounters a Ghost!"
                                                      if lives > 1
                                                        then do
                                                          putStrLn "You lost a life!"
                                                          go (Hole, complexTree) score (lives - 1) seed number -- Reset position
                                                        else do
                                                          putStrLn "Game Over!"
                                                          putStrLn $ "Final Score: " ++ show score
                                                    N _ Just_moved_Ghost _ _-> do
                                                      putStrLn "Pacman encounters a Ghost!"
                                                      if lives > 1
                                                        then do
                                                          putStrLn "You lost a life!"
                                                          go (Hole, complexTree) score (lives - 1) seed number-- Reset position
                                                        else do
                                                          putStrLn "Game Over!"
                                                          putStrLn $ "Final Score: " ++ show score
                                                    _ ->   go newZ score lives seed n

                                    Nothing -> do
                                      putStrLn $ "You cannot move " ++ dir ++ " any further."
                                      go z score lives seed n
repl :: IO ()
repl = do
        putStrLn "Welcome to Pacman.\n"
        putStrLn "You are at the root of a binary tree."
        go (Hole, complexTree) 0 3 0 number -- Start with score 0 and 3 lives

         
main = repl
