
module MoveFunctions where
import Bin

data Direction = DDown | LLeft | RRight deriving(Show,Eq)
wait :: Maybe BinZip -> Maybe BinZip 
wait bz = bz 

go_left :: Maybe BinZip -> Maybe (BinZip )
go_left (Just (c, N id tile l r)) = Just (B0 c r tile id, l)  -- focus on the left child
go_left _                  = Nothing            -- (leaf => no left child)

go_right :: Maybe BinZip  -> Maybe (BinZip)
go_right (Just (c, N id tile l r)) = Just (B1 l c tile id, r) -- focus on the right child
go_right _              = Nothing           -- (leaf => no right child)

go_down :: Maybe BinZip  -> Maybe BinZip
go_down (Just (B0 c l tile id, t)) = Just (c, N id tile t l)
go_down (Just (B1 l c tile id, t)) = Just (c, N id tile l t)
go_down  _                         = Nothing

get_id :: Maybe BinZip -> Maybe Int
get_id Nothing = Nothing
get_id  (Just (_ , L id _)) = Just id
get_id  (Just (_, N id _ _ _)) = Just id
-------------------------------------------------------------------
determine_down :: Maybe BinZip -> Maybe BinZip -> Maybe BinZip
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

 -----------------------------------------------------------------
teleport_aux :: Maybe BinZip -> Maybe Int -> Maybe BinZip -> Direction -> Bool -> Maybe (BinZip)
teleport_aux Nothing _ _ _ _= Nothing
teleport_aux bz id_wanted (source) _ True = do   
  let l1 = (go_left  bz) 
      r1 = (go_right bz) 
      d1 = (go_down  bz)
  if (get_id l1) == (id_wanted) && (not (l1 == source))
  then l1
  else if (get_id r1) == (id_wanted) && (not(r1 == source))
       then r1
       else if (get_id d1) == (id_wanted) && (not (d1 == source))
            then d1
            else case (teleport_aux l1 id_wanted source DDown False) of
                    Nothing -> case teleport_aux r1 id_wanted source DDown False of
                                  Nothing -> case determine_down_direction d1 bz of
                                                Nothing -> Nothing
                                                Just di -> case teleport_aux d1 id_wanted source di False of
                                                              Nothing -> Nothing 
                                                              bz1 -> bz1
                                  bz2 -> bz2
                    bz3 -> bz3
teleport_aux bz id_wanted source DDown False = do 
  let l1 = go_left bz 
  let r1 = go_right bz 
  if (get_id l1) == id_wanted && (not (l1 == source))
    then l1
    else if (get_id r1) == id_wanted && (not (r1 == source))
         then r1
         else case teleport_aux l1 id_wanted source DDown False of 
                        Nothing -> case teleport_aux r1 id_wanted source DDown False of  
                                      Nothing -> Nothing 
                                      bz1 -> bz1
                        bz2 -> bz2
teleport_aux bz id_wanted source LLeft False = do 
  let r1 = go_right bz 
  let d1 = go_down bz 
  if (get_id r1) == id_wanted && (not (r1 == source))
    then r1
    else if (get_id d1) == id_wanted && (not (d1 == source))
         then d1
         else case teleport_aux r1 id_wanted source DDown False of
                Nothing -> case determine_down_direction d1 bz of
                              Nothing -> Nothing
                              Just di -> case teleport_aux d1 id_wanted source di False of
                                            Nothing -> Nothing 
                                            bz1 -> bz1
                bz2 -> bz2
teleport_aux bz id_wanted source RRight False = do 
  let l1 = go_left bz 
  let d1 = go_down bz 
  if (get_id l1) == id_wanted && (not (l1 == source))
    then l1
    else if (get_id d1) == id_wanted && (not (d1 == source))
         then d1
         else case (teleport_aux l1 id_wanted source DDown False) of
                Nothing -> case (determine_down_direction d1 bz) of
                              Nothing -> Nothing
                              Just di -> case  teleport_aux d1 id_wanted source di False of
                                            Nothing -> Nothing 
                                            bz1 -> bz1
                bz2 -> bz2

teleport :: Maybe BinZip -> Maybe BinZip
teleport bz = teleport_aux bz (get_id bz) bz RRight True

