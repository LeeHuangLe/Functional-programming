
module GhostMovements where

import Bin
import MoveFunctions
import System.Random 

has_pellet :: Maybe BinZip -> Bool
has_pellet Nothing = False
has_pellet (Just (_, N _ Pellet _ _)) = True
has_pellet (Just (_, L _ Pellet ))    = True 
has_pellet (Just (_,_))               = False

has_ghost :: Maybe BinZip -> Bool
has_ghost Nothing = False
has_ghost (Just (_, N _ Ghost _ _)) = True
has_ghost (Just (_, L _ Ghost ))    = True 
has_ghost (Just (_,_))              = False

has_just_moved_ghost :: Maybe BinZip -> Bool
has_just_moved_ghost (Just (_, N _ Just_moved_Ghost _ _)) = True
has_just_moved_ghost (Just (_, L _ Just_moved_Ghost ))    = True 
has_just_moved_ghost (Just (_,_))                         = False
has_just_moved_ghost Nothing                              = False

put_ghost :: Maybe BinZip -> Maybe BinZip
put_ghost Nothing               = Nothing
put_ghost (Just (c, N i _ r l)) = Just (c, N i Ghost r l)
put_ghost (Just (c, L i _ ))    = Just (c, L i Ghost)

put_just_moved_ghost :: Maybe BinZip -> Maybe BinZip
put_just_moved_ghost Nothing               = Nothing
put_just_moved_ghost (Just (c, N i _ r l)) = Just (c, N i Just_moved_Ghost r l)
put_just_moved_ghost (Just (c, L i _ ))    = Just (c, L i Just_moved_Ghost)

remove_ghost :: Maybe BinZip -> Maybe BinZip
remove_ghost (Just (c, N i Ghost r l)) = Just (c, N i EmptyTile r l)
remove_ghost (Just (c, L i Ghost))     = Just (c, L i EmptyTile)
remove_ghost (Just x)                  = Just x
remove_ghost Nothing                   = Nothing

-- given two nodes b and c, possibly neighbors, if c is a child of b, then returns which child.
-- useful if we are navegating in the tree 
-- same for last function, but determines the direction of the child

moveGhost_case :: Maybe BinZip -> Int -> Maybe BinZip
moveGhost_case Nothing _ = Nothing
moveGhost_case b 1 = case (go_left (remove_ghost b)) of
                        Nothing -> moveGhost_case b 3
                        l -> if ((has_ghost l) || (has_just_moved_ghost l)) || (has_pellet l)
                                  then b
                                  else case (go_down (put_just_moved_ghost l)) of
                                          Nothing -> b
                                          l1 -> l1
moveGhost_case b 2 = case (go_right (remove_ghost b)) of
                        Nothing -> moveGhost_case b 3
                        l -> if ((has_ghost l) || (has_just_moved_ghost l)) || (has_pellet l)
                                  then b
                                  else case (go_down (put_just_moved_ghost l)) of
                                          Nothing -> b
                                          l1 -> l1
moveGhost_case b 3 = case (go_down (remove_ghost b)) of
                        Nothing -> moveGhost_case b 1
                        l -> if ((has_ghost l) || (has_just_moved_ghost l) || has_pellet l)
                                  then b
                                  else case (determine_down (put_just_moved_ghost l) b) of
                                          Nothing -> b
                                          l1 -> l1


getRandomInt  :: Int -> Int -> StdGen -> Int 
getRandomInt low high gen = fst (randomR (low ,high) gen)

moveGhost :: Maybe BinZip -> Int -> Maybe BinZip
moveGhost Nothing _ = Nothing
moveGhost b seed = if (has_ghost b)
                   then moveGhost_case b (getRandomInt 1 3 (mkStdGen seed))
                   else if (has_just_moved_ghost b)
                        then put_ghost b
                        else b

updateTree :: Maybe BinZip -> Bool -> Direction -> Int -> Maybe BinZip
updateTree Nothing _ _ _       = Nothing
updateTree b True _ seed       = case go_down (updateTree (go_left b) False DDown seed) of
                                     Nothing -> case go_down (updateTree (go_right b) False DDown seed) of
                                                   Nothing -> case (determine_down_direction (go_down b) b) of 
                                                                  Nothing -> (moveGhost b seed)
                                                                  Just di -> case determine_down (updateTree (go_down b) False di seed) b of
                                                                                Nothing -> (moveGhost b seed)
                                                                                x3 -> (moveGhost x3 seed)
                                                   x1 -> case (determine_down_direction (go_down x1) x1) of
                                                                  Nothing -> (moveGhost x1 seed)
                                                                  Just di -> case determine_down (updateTree (go_down x1) False di seed) b of
                                                                                Nothing -> (moveGhost x1 seed)
                                                                                x3 -> (moveGhost x3 seed)
                                     x2 ->  case go_down (updateTree (go_right x2) False DDown seed) of
                                                   Nothing -> case (determine_down_direction (go_down x2) b) of 
                                                                  Nothing -> (moveGhost x2 seed)
                                                                  Just di -> case determine_down (updateTree (go_down x2) False di seed) b of
                                                                                Nothing -> (moveGhost x2 seed )
                                                                                x3 -> (moveGhost x3 seed)
                                                   x1 -> case (determine_down_direction (go_down x1) b) of
                                                                  Nothing -> (moveGhost x1 seed)
                                                                  Just di -> case determine_down (updateTree (go_down x1) False di seed) b of
                                                                                Nothing -> (moveGhost x1 seed )
                                                                                x3 -> (moveGhost x3 seed)

updateTree b False DDown seed = case go_down (updateTree (go_left b) False DDown seed) of
                                    Nothing -> case go_down (updateTree (go_right b) False DDown seed) of
                                                  Nothing -> (moveGhost b seed)
                                                  x2 -> (moveGhost x2 seed) 
                                    x1 -> case go_down (updateTree (go_right x1) False DDown seed) of
                                                  Nothing -> (moveGhost x1 seed)
                                                  x3 -> (moveGhost x3 seed)

updateTree b False RRight seed = case go_down (updateTree (go_left b) False DDown seed) of 
                                    Nothing -> case (determine_down_direction (go_down b) b ) of 
                                                  Nothing -> (moveGhost b seed)
                                                  Just di -> case determine_down (updateTree (go_down b) False di seed) b of
                                                              Nothing -> (moveGhost b seed)
                                                              x3 -> (moveGhost x3 seed)
                                    x1 -> case (determine_down_direction (go_down x1) x1) of 
                                                  Nothing -> (moveGhost x1 seed)
                                                  Just di -> case determine_down (updateTree (go_down x1) False di seed) x1 of
                                                              Nothing -> (moveGhost x1 seed)
                                                              x3 -> (moveGhost x3 seed)

updateTree b False LLeft seed = case go_down (updateTree (go_right b) False DDown seed) of 
                                    Nothing -> case (determine_down_direction (go_down b) b ) of 
                                                  Nothing -> (moveGhost b seed)
                                                  Just di -> case determine_down (updateTree (go_down b) False di seed) b of
                                                              Nothing -> (moveGhost b seed)
                                                              x3 -> (moveGhost x3 seed)
                                    x1 -> case (determine_down_direction (go_down x1) x1) of 
                                                  Nothing -> (moveGhost x1 seed)
                                                  Just di -> case determine_down (updateTree (go_down x1) False di seed) x1 of
                                                              Nothing -> (moveGhost x1 seed)
                                                              x3 -> (moveGhost x3 seed)


