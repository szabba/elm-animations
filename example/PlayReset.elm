module PlayReset exposing (main)

import String
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Time)
import Tuple2
import Window
import Animation exposing (Animation)
import Animation.App as App
import Helpers


main : Program Never
main =
    App.program { init = playToResetAndBack |> Animation.map toSvg, loop = True }


type alias Params =
    { alpha : Float
    , gamma : Float
    , r : Float
    , s : Float
    , d : Float
    }


toSvg : Params -> Window.Size -> Svg msg
toSvg params { width, height } =
    [ box, triangle, tail ]
        |> List.map ((|>) params)
        |> S.g [ SA.transform <| "scale(" ++ toString (min width height // 3) ++ ")" ]


box : a -> Svg msg
box _ =
    S.rect
        [ SA.x "-1"
        , SA.y "-1"
        , SA.width "2"
        , SA.height "2"
        , SA.fill "transparent"
        ]
        []


playToResetAndBack : Animation Params
playToResetAndBack =
    (playToReset |> Helpers.freezeEndFor Time.second)
        `Animation.append` (resetToPlay |> Helpers.freezeEndFor Time.second)


playToReset : Animation Params
playToReset =
    (3 * Time.second)
        |> Animation.interval
        |> Animation.map
            (\t ->
                { alpha = t * (pi - pi / 6 + pi / 2)
                , gamma = t * (8 / 5 * pi)
                , r = t
                , d = 0.75 * (-1 + t)
                , s = -0.5 * t + 1
                }
            )
        |> Animation.continue Time.second always


resetToPlay : Animation Params
resetToPlay =
    playToReset
        |> Animation.reverse
        |> Animation.reset


type alias Triangle a =
    { a
        | alpha : Float
        , r : Float
        , s : Float
        , d : Float
    }


triangle : Triangle a -> Svg msg
triangle { alpha, r, s, d } =
    let
        transform =
            [ "rotate(" ++ (toString <| toDegrees -alpha) ++ ")"
            , "translate(0 " ++ toString r ++ ")"
            , "scale(" ++ toString s ++ ")"
            , "translate(" ++ toString d ++ ")"
            ]
                |> List.intersperse " "
                |> String.concat

        points =
            [ (,) 0 -1, (,) 0 1, (,) 1.5 0 ]
                |> pointsToString
    in
        S.g [ SA.transform transform ]
            [ S.polygon [ SA.points points, SA.fill fillColor ] [] ]


type alias Tail a =
    { a
        | r : Float
        , alpha : Float
        , gamma : Float
    }


tail : Tail a -> Svg msg
tail ({ r, alpha, gamma } as tail) =
    S.g
        [ SA.transform <| "rotate(" ++ (toString <| (+) 89 << toDegrees <| -alpha) ++ ")"
        ]
        [ S.path
            [ SA.d <| tailPath tail
            , SA.stroke fillColor
            , SA.strokeWidth "0.3"
            , SA.fillOpacity "0"
            ]
            []
        ]


tailPath : Tail a -> String
tailPath { r, alpha, gamma } =
    let
        ( startPoint, radii ) =
            ( " " ++ toString r ++ " 0 "
            , " " ++ toString r ++ " " ++ toString r ++ " "
            )

        longerArc =
            if gamma > pi then
                " 1 "
            else
                " 0 "

        ( x, y ) =
            fromPolar ( r, gamma )

        endPoint =
            " " ++ toString x ++ " " ++ toString y ++ " "
    in
        ("M " ++ startPoint)
            ++ ("A " ++ radii ++ " 0 ")
            ++ (longerArc ++ " 1 ")
            ++ (toString x ++ " " ++ toString y)


pointsToString : List ( a, a ) -> String
pointsToString =
    List.map (Tuple2.mapBoth toString >> \( x, y ) -> x ++ " " ++ y)
        >> List.intersperse ", "
        >> String.concat


toDegrees : Float -> Float
toDegrees rads =
    rads / pi * 180


fillColor : String
fillColor =
    "#DD7700"
