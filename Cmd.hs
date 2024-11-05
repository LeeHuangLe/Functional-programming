module Cmd where

data Cmd = Go_Left | Go_Right | Go_Down | Quit
  deriving (Show,Read)
