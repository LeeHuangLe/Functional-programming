import Bin
import Cmd
import Parser

import System.IO
import Control.Concurrent (threadDelay)

-- a small binary tree
smallTree :: Bin
smallTree = N Pacman 
             (L Pellet) 
             (N Wall (L EmptyTile) (L Pacman))

-- the top-level interactive loop
repl :: IO ()
repl = do
  putStrLn "Welcome Pacman.\n"
  putStrLn "You are at the root of a binary tree."
  go (Hole,smallTree)
  where
    go :: BinZip Int -> IO ()
    go z = do
      let (_, t) = z                                          -- give the player some information
      case z of                                        -- about the current position in the tree
        L tile -> putStrLn $ "You see a leaf. It contains: " ++ show tile
        N tile _ _ -> putStrLn $ "You see a node. It contains: " ++ show tile
      putStr "> "                                      -- print the prompt
      hFlush stdout                                    -- flush standard output
      line <- getLine                                  -- get a line of input
      case parseInput parseCmd line of                 -- parse the input
        Nothing -> do
          putStrLn "I'm sorry, I do not understand."
          go z
        Just Go_Left -> tryMove go_left "left" z
        Just Go_Right -> tryMove go_right "right" z
        Just Go_Down -> tryMove go_down "down" z
        Just Quit -> do
          putStrLn "Okay."
          putStrLn "You ended the game over here:\n"
          putStrLn (drawBinZip z)
          putStrLn "Goodbye."

    handleTile :: Tile -> IO ()
    handleTile Pacman = putStrLn "You are Pacman."
    handleTile Pellet = putStrLn "You see a Pellet. Pacman eats it!"
    handleTile Wall = putStrLn "You see a Wall. You cannot move here."
    handleTile EmptyTile = putStrLn "You see an empty space."

    tryMove :: (BinZip -> Maybe BinZip) -> String -> BinZip -> IO ()
    tryMove move dir z =
      case move z of
        Just newZ -> case snd newZ of
                       L Wall -> do
                         putStrLn $ "You cannot move " ++ dir ++ ", there's a Wall."
                         go z
                       N Wall _ _ -> do
                         putStrLn $ "You cannot move " ++ dir ++ ", there's a Wall."
                         go z
                       _ -> go newZ
        Nothing -> do
          putStrLn $ "You cannot move " ++ dir ++ " any further."
          go z
          
main = repl
