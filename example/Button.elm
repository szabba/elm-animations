module Button exposing (..)

import String
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time
import Animation exposing (Animation)
import Helpers


type alias Model =
    { boxSize : Float
    , sigma : Float
    , triangle : Triangle
    }


type alias Triangle =
    { alpha : Float
    , r : Float
    , s : Float
    , d : Float
    }


triangleAnimation : Animation (Svg msg)
triangleAnimation =
    let
        dt =
            Time.second
    in
        dt
            |> Animation.interval
            |> Animation.map
                (\t ->
                    { alpha = t / dt * (pi - pi / 6 + pi / 2)
                    , r = t / dt
                    , d = -1 + t / dt
                    , s = -0.5 * t / dt + 1
                    }
                )
            |> Animation.map viewTriangle


viewTriangle : Triangle -> Svg msg
viewTriangle params =
    let
        transform =
            [ "rotate(" ++ (toString <| toDegrees params.alpha) ++ ")"
            , "translate(0 " ++ toString params.r ++ ")"
            , "scale(" ++ toString params.s ++ ")"
            , "translate(" ++ toString params.d ++ ")"
            ]
                |> List.intersperse " "
                |> String.concat

        points =
            [ (,) 0 -1, (,) 0 1, (,) 1 0 ]
                |> Helpers.pointsToString
    in
        S.g [ SA.transform transform ]
            [ S.polygon [ SA.points points ] [] ]


toDegrees =
    (*) -180 >> flip (/) pi
