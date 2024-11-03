-- data types for binary trees + one-hole contexts and zippers, as well
-- as some pretty-printing routines

module Bin where

import Data.Tree

--The type of information on each Node
data Tile = Wall | Path | Pellet | Ghost
  deriving (Eq, Show)

--The type of binary tree 
data BinaryTree = L 
                | Node Tile BinaryTree BinaryTree
  deriving (Eq, Show)

-- a "one-hole context" for a binary tree may be thought of as a
-- binary tree with a hole for another binary tree
data BinCxt = Hole
            | B0 BinCxt Bin
            | B1 Bin BinCxt
  deriving (Show, Eq)

-- Plugging a one-hole context with a binary tree to produce a binary
-- tree is accomplished by the following function
plug :: BinCxt -> Bin -> Bin
plug Hole      t = t
plug (B0 c t2) t = plug c (Node EmptyTile t t2)
plug (B1 t1 c) t = plug c (Node EmptyTile t1 t)

-- A zipper is a pair of a one-hole context c and a tree t, which we
-- think of as defining a pointer to t as a subtree of u = plug c t.
type BinZip = (BinCxt, Bin)
-- (The terminology comes from Gérard Huet's paper, "The Zipper".)

-- The following functions implement moving the pointer up to the
-- left child, up to the right child, or down to the parent of a
-- subtree.  (Note that these operations are only partial, i.e., return a
-- Maybe type, since the subtree may not have a child or a parent.)

go_left :: BinZip a -> Maybe (BinZip a)
go_left (c, Node _ l r) = Just (B0 c r, l)  -- focus on the left child
go_left _ = Nothing            -- (leaf => no left child)

go_right :: BinZip a -> Maybe (BinZip a)
go_right (c, Node _ l r) = Just (B1 l c, r) -- focus on the right child
go_right (c,L _)     = Nothing           -- (leaf => no right child)

go_down :: BinZip a -> Maybe (BinZip a)
go_down (B0 c r, t) = Just (c, Node EmptyTile t r)    -- focus on parent *from* left child
go_down (B1 l c, t) = Just (c, Node EmptyTile l t)    -- focus on parent *from* right child
go_down _ = Nothing            -- (root => no parent)

-- It is also easy to implement operations that perform simple edits,
-- such as say grafting another tree off to the left or right of the
-- the subtree in focus.

graft_left :: Bin -> BinZip -> BinZip
graft_left g (c, t) = (c, Node EmptyTile g t)
graft_right :: Bin -> BinZip -> BinZip
graft_right g (c, t) = (c, Node EmptyTile t g)

-- Finally, we include some pretty-printing routines for binary trees
-- and binary tree zippers.

-- We make use of drawTree :: Tree String -> String from the Data.Tree
-- module, after first defining some conversion routines from Bin's
-- and BinZip's to Tree String's, which also relies on interpreting a
-- BinCxt as a function Tree String -> Tree String.

treeFromBin :: Bin -> Tree String
treeFromBin Empty = Node "Empty" []
treeFromBin (Node tile left right) =
  Node (show tile) [treeFromBin left, treeFromBin right]

treeCxtFromBinCxt :: BinCxt -> Tree String -> Tree String
treeCxtFromBinCxt Hole t = t
treeCxtFromBinCxt (B0 c t2) t = treeCxtFromBinCxt c (Node "*" [t, treeFromBin t2])
treeCxtFromBinCxt (B1 t1 c) t = treeCxtFromBinCxt c (Node "*" [treeFromBin t1, t])

treeFromBinZip :: BinZip -> Tree String
treeFromBinZip (c, t) =
  let t' = treeFromBin t
      marker = " @ <-- you"
  in treeCxtFromBinCxt c (t' { rootLabel = rootLabel t' ++ marker })

drawBin :: Show a => Bin a -> String
drawBin = drawTree . treeFromBin

drawBinZip :: Show a => BinZip a -> String
drawBinZip = drawTree . treeFromBinZip
