
module GhostMovements where

import Bin
import System.Random 


has_ghost :: Maybe BinZip -> Bool
has_ghost Nothing = False
has_ghost (Just (_, N _ Ghost _ _)) = True
has_ghost (Just (_, L _ Ghost )) = True 
has_ghost (Just (_,_))  = False

put_ghost :: Maybe BinZip -> Maybe BinZip
put_ghost Nothing = Nothing
put_ghost (Just (c, N i _ r l)) = Just (c, N i Ghost r l)
put_ghost (Just (c, L i _ )) = Just (c, L i Ghost)

remove_ghost :: BinZip -> BinZip
remove_ghost (c, N i Ghost r l) = (c, N i EmptyTile r l)
remove_ghost x = x

is_leaf ::  BinZip -> Bool
is_leaf (c, L _ _) = True
is_leaf (c, N _ _ _ _) = False

data Direction = DDown | LLeft | RRight

determine_down :: Maybe BinZip -> (Maybe BinZip) -> Maybe BinZip
determine_down Nothing _ = Nothing
determine_down _ Nothing = Nothing
determine_down b c = let x = go_left b 
                         y = go_right b
                      in if ((get_id y) == (get_id c))
                         then y
                         else if ((get_id x) == (get_id c))
                              then x
                              else Nothing


determine_down_direction :: Maybe BinZip -> Maybe BinZip -> Maybe Direction
determine_down_direction _ Nothing = Nothing 
determine_down_direction Nothing _ = Nothing
determine_down_direction b c = let x = go_left b
                                   y = go_right b
                                   in if ((get_id y) == (get_id c))
                                      then (Just RRight)
                                      else if ((get_id x) == (get_id c))
                                           then (Just LLeft)
                                           else Nothing


moveGhost_case :: Maybe BinZip -> Int -> Maybe BinZip
moveGhost_case Nothing _ = Nothing
moveGhost_case b 1 = case (go_left b) of
                        Nothing -> b
                        l -> if (has_ghost l)
                                  then b
                                  else case (go_down (put_ghost l)) of
                                          Nothing -> b
                                          l1 -> l1
moveGhost_case b 2 = case (go_right b) of
                        Nothing -> b
                        l -> if (has_ghost l)
                                  then b
                                  else case (go_down (put_ghost l)) of
                                          Nothing -> b
                                          l1 -> l1
moveGhost_case b 3 = case (go_down b) of
                        Nothing -> b
                        l -> if (has_ghost l)
                                  then b
                                  else case (determine_down (put_ghost l) b) of
                                          Nothing -> b
                                          l1 -> l1


moveGhost :: Maybe BinZip -> Maybe BinZip
moveGhost b = moveGhost_case b 1


updateTree :: Maybe BinZip -> Bool -> Direction -> Maybe BinZip
updateTree Nothing _ _ = Nothing
updateTree b True _       = case go_down (updateTree (go_left b) False DDown) of
                                     Nothing -> case go_down (updateTree (go_right b) False DDown) of
                                                   Nothing -> case (determine_down_direction (go_down b) b) of 
                                                                  Nothing -> (moveGhost b)
                                                                  Just di -> case determine_down (updateTree (go_down b) False di) b of
                                                                                Nothing -> (moveGhost b)
                                                                                x3 -> (moveGhost x3)
                                                   x1 -> case (determine_down_direction (go_down x1) b) of
                                                                  Nothing -> (moveGhost x1)
                                                                  Just di -> case determine_down (updateTree (go_down x1) False di) b of
                                                                                Nothing -> (moveGhost x1)
                                                                                x3 -> (moveGhost x3)
                                     x2 ->  case go_down (updateTree (go_right x2) False DDown) of
                                                   Nothing -> case (determine_down_direction (go_down x2) b ) of 
                                                                  Nothing -> (moveGhost x2)
                                                                  Just di -> case determine_down (updateTree (go_down x2) False di) b of
                                                                                Nothing -> (moveGhost b)
                                                                                x3 -> (moveGhost x3)
                                                   x1 -> case (determine_down_direction (go_down x1) b) of
                                                                  Nothing -> (moveGhost x1)
                                                                  Just di -> case determine_down (updateTree (go_down x1) False di) b of
                                                                                Nothing -> (moveGhost x1)
                                                                                x3 -> (moveGhost x3)

updateTree b False DDown  = case go_down (updateTree (go_left b) False DDown) of
                                    Nothing -> case go_down (updateTree (go_right b) False DDown) of
                                                  Nothing -> (moveGhost b)
                                                  x2 -> (moveGhost x2) 
                                    x1 -> case go_down (updateTree (go_right x1) False DDown) of
                                                  Nothing -> (moveGhost b)
                                                  x3 -> (moveGhost x3)

updateTree b False RRight = case go_down (updateTree (go_left b) False DDown) of 
                                    Nothing -> case go_down (updateTree (go_right b) False DDown) of
                                                  Nothing -> (moveGhost b)
                                                  x2 -> (moveGhost x2)
                                    x1 -> case (determine_down_direction (go_down x1) b) of 
                                                  Nothing -> (moveGhost b)
                                                  Just di -> case determine_down (updateTree (go_down x1) False di) b of
                                                              Nothing -> (moveGhost b)
                                                              x3 -> (moveGhost x3)

updateTree b False LLeft = case go_down (updateTree (go_right b) False DDown) of 
                                    Nothing -> case go_down (updateTree (go_left b) False DDown) of
                                                  Nothing -> (moveGhost b)
                                                  x2 -> (moveGhost x2)
                                    x1 -> case (determine_down_direction (go_down x1) b) of 
                                                  Nothing -> (moveGhost b)
                                                  Just di -> case determine_down (updateTree (go_down x1) False di) b of
                                                              Nothing -> (moveGhost b)
                                                              x3 -> (moveGhost x3)




