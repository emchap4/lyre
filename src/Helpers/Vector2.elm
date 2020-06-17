module Helpers.Vector2 exposing (..)

type alias Vector2 = 
    { x : Float
    , y : Float}


isPointAbove : Vector2 -> Vector2 -> Bool
isPointAbove v1 v2 =
    v1.y < v2.y
    -- it's backwards because the y-axis increases from top to bottom in HTML