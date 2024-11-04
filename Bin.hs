-- data types for binary trees + one-hole contexts and zippers, as well
-- as some pretty-printing routines

module Bin where

import Data.Tree

--The type of information on each Node
data Tile = Wall | Path | Pellet | Ghost | EmptyTile
  deriving (Eq, Show)

--The type of binary tree 
data Bin  = L Int Tile 
            | N Int Tile Bin Bin

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
plug  Hole      t = t
plug (B0 c t2) t = plug c (N EmptyTile t t2)
plug (B1 t1 c) t = plug c (N EmptyTile t1 t)

-- A zipper is a pair of a one-hole context c and a tree t, which we
-- think of as defining a pointer to t as a subtree of u = plug c t.
type BinZip = (BinCxt, Bin)
-- (The terminology comes from Gérard Huet's paper, "The Zipper".)

-- The following functions implement moving the pointer up to the
-- left child, up to the right child, or down to the parent of a
-- subtree.  (Note that these operations are only partial, i.e., return a
-- Maybe type, since the subtree may not have a child or a parent.)

go_left :: BinZip -> Maybe (BinZip )
go_left (c, N _ _ l r) = Just (B0 c r, l)  -- focus on the left child
go_left _ = Nothing            -- (leaf => no left child)

go_right :: BinZip  -> Maybe (BinZip)
go_right (c, N _ _ l r) = Just (B1 l c, r) -- focus on the right child
go_right _              = Nothing           -- (leaf => no right child)

go_down :: BinZip  -> Maybe (BinZip )
go_down (B0 c r, t) = Just (c, N EmptyTile t r)    -- focus on parent *from* left child
go_down (B1 l c, t) = Just (c, N EmptyTile l t)    -- focus on parent *from* right child
go_down _ = Nothing            -- (root => no parent)


get_id :: Maybe BinZip -> Maybe Int
get_id Nothing = Nothing
get_id (Just (_ , L id _)) = Just id
get_id (Just (_, N id _ _ _)) = Just id


{-
Bin_equal :: Bin -> Bin -> Bool
Bin_equal (N _ _ b1 b2) (L _ _ ) = False
Bin_equal (N a b b1 b2) (N c d b3 b4) = (Bin_equal b1 b3) && (Bin_equal b2 b4) && (a == b) && (Tile_equal b d)

BinCtx_equal :: BinCxt -> BinCxt -> Bool
BinCtx_equal Hole Hole = True
BinCtx_equal _ Hole = False
BinCtx_equal Hole _ = False 
BinCtx_equal (B0 _ _) (B1 _ _) = False 
BinCtx_equal (B1 _ _) (B0 _ _) = False 
BinCtx_equal (B0 c1 b1) (B0 c2 b2) = (BinCtx_equal c1 c2) && (Bin_equal b1 b2)
BinCtx_equal (B1 b1 c1) (B1 b2 c2) = (BinCtx_equal c1 c2) && (Bin_equal b1 b2)
-}

binzip_equal :: BinZip -> BinZip -> Bool
binzip_equal ((a, b)) ((c, d)) = ((a == c) && (b == d))

teleport :: Maybe BinZip -> Int -> BinZip -> Maybe (BinZip)
teleport Nothing _ _ = Nothing
teleport (Just bz) id_wanted (source) = do   
  let l1 = (go_left  bz) 
      r1 = (go_right  bz) 
      d1 = (go_down  bz)
  if (get_id l1) == (Just id_wanted)
  then l1
  else if (get_id r1) == (Just id_wanted)
       then r1
       else if (get_id d1) == (Just id_wanted)
            then d1
            else
              let l = teleport (l1) id_wanted source
                  r = teleport (r1) id_wanted source
                  d = teleport (d1) id_wanted source
              in case l of 
                  Nothing -> case r of 
                                Nothing -> case d of 
                                              Nothing -> Nothing
                                              (Just bz1) -> if binzip_equal bz1 source
                                                            then Nothing 
                                                            else (Just bz1)
                                (Just bz2) -> if binzip_equal bz2 source 
                                              then Nothing 
                                              else (Just bz2)

                  (Just bz3) -> if binzip_equal bz3 source 
                                then Nothing 
                                else (Just bz3)

                                 -- It is also easy to implement operations that perform simple edits,
-- such as say grafting another tree off to the left or right of the
-- the subtree in focus.

graft_left :: Bin -> BinZip -> BinZip
graft_left g (c, t) = (c, N EmptyTile g t)
graft_right :: Bin -> BinZip -> BinZip
graft_right g (c, t) = (c, N EmptyTile t g)

-- Finally, we include some pretty-printing routines for binary trees
-- and binary tree zippers.

-- We make use of drawTree :: Tree String -> String from the Data.Tree
-- module, after first defining some conversion routines from Bin's
-- and BinZip's to Tree String's, which also relies on interpreting a
-- BinCxt as a function Tree String -> Tree String.

treeFromBin :: Bin -> Tree String
treeFromBin EmptyTile = N "Empty" []
treeFromBin (N tile left right) =
  N (show tile) [treeFromBin left, treeFromBin right]

treeCxtFromBinCxt :: BinCxt -> Tree String -> Tree String
treeCxtFromBinCxt Hole t = t
treeCxtFromBinCxt (B0 c t2) t = treeCxtFromBinCxt c (N "*" [t, treeFromBin t2])
treeCxtFromBinCxt (B1 t1 c) t = treeCxtFromBinCxt c (N "*" [treeFromBin t1, t])

treeFromBinZip :: BinZip -> Tree String
treeFromBinZip (c, t) =
  let t' = treeFromBin t
      marker = " @ <-- you"
  in treeCxtFromBinCxt c (t' { rootLabel = rootLabel t' ++ marker })

drawBin :: Bin -> String
drawBin = drawTree . treeFromBin

drawBinZip :: BinZip -> String
drawBinZip = drawTree . treeFromBinZip
