module Helpers.Grid exposing (GridPosition, sameGridPosition)


type alias GridPosition = { x : Int
 , y : Int }

sameGridPosition : GridPosition -> GridPosition -> Bool
sameGridPosition pos1 pos2 =
    pos1.x == pos2.x && pos1.y == pos2.y