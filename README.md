# Project Description - BinPacman
This is a Game that emulates Pacman in a binary tree.
The objective is to collect all pellets without losing.
Whenever you touch a ghost (i.e. arrive at the same node as them), you lose a life. If you lose all your 3 lives, you lose.
The commands are:
 - climb left (follows to the left node of the bin tree);
 - climb right (follows to the right node of the bin tree);
 - climb down (follows back up in the tree);
 - wait (stay still);
 - teleport (teleport to closest node of same id);
 - show (shows binary tree).

## Most important features:
 - Each node and leaf in a binary node contain information of whether it has something in it or not;
 - Teleporting move was added as a way of adding cycles to binary tree;
 - Waiting move was added so it adds more possibilities of strategies (sometimes not moving is optimal);
 - Ghost movements was added by:
    1) They randomly move to one of adjacent nodes, not including teleporting;
    2) They may wait still because of the way it was implemented (check Just_moved_Ghost and Ghost constructors);
    3) Function updateTree scans the tree starting from current pacman position, and it navigates in all directions (down, left and right) without scanning the same node twice and it moves a ghost, if possible;
    4) They don't go to nodes with pellets on it;
    5) To avoid weird behaviour, we added a type called Just_moved_Ghost, which prevents the ghost to be scanned multiple times in case it moves in the direction of the scanning.
 - We update a seed every iteration of go function, so we have random ghost movements;
 - When all pellets are collected, The game ends automatically;
 - The tree used can be changed by altering the complexTree and the number variables, which indicates the number of pellets in the tree. Both are in file BinPacman.hs.
## Files and contents:
 - Bin.hs: Binary Tree structures;
 - Cmd.hs: Commands constructors for commands;
 - Parser.hs: Parser for user input;
 - MoveFunctions.hs: Functions to deal with movements;
 - GhostMovements.hs: Functions to deal with ghost movements;
 - BinTreeWorld.hs: Run game.

## How to build

There may be different ways to build the project:
 - Make sure that all necessary modules are installed in your local machine;
 - Additional module used: 'System.Random';
 - On Windows, use the Makefile with the flag 'GHC_FLAGS=-package containers';
 - On Linux, use the Makefile with the flag 'GHC_FLAGS=-dynamic'.

## Members:
 - Bruno Fernandes Iorio
 - Lee Huang
 - Timofei Fedoseev




