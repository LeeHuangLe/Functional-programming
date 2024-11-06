import GhostMovements
import Bin 


tree :: Bin 
tree = N 3 Pacman 
        (N 4 EmptyTile (L 1 EmptyTile) (L 2 Ghost))
        (L 5 Pellet)
t :: BinZip
t = (Hole, tree) 



