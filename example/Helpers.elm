module Helpers exposing (..)

import String
import Tuple2


pointsToString : List ( a, a ) -> String
pointsToString =
    List.map (Tuple2.mapBoth toString >> \( x, y ) -> x ++ " " ++ y)
        >> List.intersperse ", "
        >> String.concat


toDegrees : Float -> Float
toDegrees rad =
    rad * 180 / pi
