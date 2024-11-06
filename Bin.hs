-- data types for binary trees + one-hole contexts and zippers, as well
-- as some pretty-printing routines

module Bin where

import Data.Tree
import Control.Monad (mplus)

--The type of information on each Node
data Tile = Wall | Pellet | Ghost | EmptyTile | Pacman | Just_moved_Ghost
  deriving (Eq, Show)

--The type of binary tree 
data Bin  = L Int Tile 
            | N Int Tile Bin Bin
  deriving (Eq, Show)

-- a "one-hole context" for a binary tree may be thought of as a
-- binary tree with a hole for another binary tree
data BinCxt = Hole
            | B0 BinCxt Bin Tile Int
            | B1 Bin BinCxt Tile Int
            
  deriving (Show, Eq)

-- Plugging a one-hole context with a binary tree to produce a binary
-- tree is accomplished by the following function
plug :: BinCxt -> Bin -> Bin
plug Hole t              = t
plug (B0 c t1 tile id) t = plug c (N id tile t t1)
plug (B1 t1 c tile id) t = plug c (N id tile t1 t)

-- A zipper is a pair of a one-hole context c and a tree t, which we
-- think of as defining a pointer to t as a subtree of u = plug c t.
type BinZip = (BinCxt, Bin) 

-- (The terminology comes from Gérard Huet's paper, "The Zipper".)

-- The following functions implement moving the pointer up to the
-- left child, up to the right child, or down to the parent of a
-- subtree.  (Note that these operations are only partial, i.e., return a
-- Maybe type, since the subtree may not have a child or a parent.)

filter_Ghost :: Tile -> Tile 
filter_Ghost Just_moved_Ghost = Ghost 
filter_Ghost x = x

removeTile :: BinZip -> BinZip 
removeTile ((c, N id _ t t1)) = (c, N id EmptyTile t t1)
removeTile ((c, L id _)) = (c,L id EmptyTile)

-- It is also easy to implement operations that perform simple edits,
-- such as say grafting another tree off to the left or right of the
-- the subtree in focus.

{-graft_left :: Bin -> BinZip -> BinZip
graft_left g (c, t) = (c, N EmptyTile g t)
graft_right :: Bin -> BinZip -> BinZip
graft_right g (c, t) = (c, N EmptyTile t g)-}

-- Finally, we include some pretty-printing routines for binary trees
-- and binary tree zippers.

-- We make use of drawTree :: Tree String -> String from the Data.Tree
-- module, after first defining some conversion routines from Bin's
-- and BinZip's to Tree String's, which also relies on interpreting a
-- BinCxt as a function Tree String -> Tree String.

treeFromBin :: Bin -> Tree String
treeFromBin (L id tile) = Node (show id ++ ": " ++ show (filter_Ghost tile)) []
treeFromBin (N id tile left right) =
  Node (show id ++ ": " ++ show (filter_Ghost tile)) [treeFromBin left, treeFromBin right]

treeCxtFromBinCxt :: BinCxt -> Tree String -> Tree String
treeCxtFromBinCxt Hole t = t
treeCxtFromBinCxt (B0 c t2 tile id) t = treeCxtFromBinCxt c (Node "*" [t, treeFromBin t2])
treeCxtFromBinCxt (B1 t1 c tile id) t = treeCxtFromBinCxt c (Node "*" [treeFromBin t1, t] )

treeFromBinZip :: BinZip -> Tree String
treeFromBinZip (c, t) =
  let t' = treeFromBin t
      marker = " @ <-- you"
  in treeCxtFromBinCxt c (t' { rootLabel = rootLabel t' ++ marker })

drawBin :: Bin -> String
drawBin = drawTree . treeFromBin

drawBinZip :: BinZip -> String
drawBinZip = drawTree . treeFromBinZip
