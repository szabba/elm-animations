module Pulse exposing (..)

import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Time)
import Window
import Animation exposing (Animation)
import Animation.App as App
import Helpers


main =
    App.program
        { init = pulse |> Animation.map toSvg
        , loop = True
        }


toSvg : Float -> Window.Size -> Svg msg
toSvg t { width, height } =
    let
        maxRadius =
            (min width height // 3)
                |> toFloat
    in
        S.circle
            [ SA.r <| toString <| t * maxRadius
            , SA.cx "0"
            , SA.cy "0"
            , SA.stroke "#000000"
            , SA.fillOpacity "0"
            , SA.strokeWidth "5"
            ]
            []


pulse : Animation Float
pulse =
    [ halfPulse |> Helpers.freezeEndFor (Time.second / 2)
    , reverseHalfPulse
    ]
        |> List.foldl Animation.append (Animation.immediately 0)


halfPulse : Animation Float
halfPulse =
    (3 * Time.second)
        |> Animation.interval
        |> Animation.map ((*) (pi / 2) >> sin)


reverseHalfPulse : Animation Float
reverseHalfPulse =
    halfPulse
        |> Animation.reverse
        |> Animation.reset
