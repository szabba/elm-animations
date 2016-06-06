module Button exposing (Model, init, needsAnimating, Msg, update, animate, view)

import Html.Events as HE
import String
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Time)
import Animation exposing (Animation)
import Helpers exposing (toDegrees)


-- MODEL


type alias Model msg =
    { state : State
    , onPlay : msg
    , onReset : msg
    , fillColor : String
    }


type State
    = Plain PlainState
    | AnimatingTo PlainState (Animation Params)


type PlainState
    = Play
    | Reset


init : { fillColor : String, onPlay : msg, onReset : msg } -> Model msg
init { fillColor, onPlay, onReset } =
    { state = Plain Play
    , onPlay = onPlay
    , onReset = onReset
    , fillColor = fillColor
    }


needsAnimating : Model msg -> Bool
needsAnimating { state } =
    case state of
        AnimatingTo _ _ ->
            True

        _ ->
            False


type alias Params =
    { alpha : Float
    , gamma : Float
    , r : Float
    , s : Float
    , d : Float
    }



-- UPDATE


type Msg
    = Clicked
    | NoOp


update : Msg -> Model msg -> ( Model msg, Maybe msg )
update msg model =
    case msg of
        Clicked ->
            case model.state of
                Plain Play ->
                    ( { model | state = playToReset |> AnimatingTo Reset }
                    , Just model.onPlay
                    )

                Plain Reset ->
                    ( { model | state = resetToPlay |> AnimatingTo Play }
                    , Just model.onReset
                    )

                AnimatingTo Play currAnimation ->
                    ( { model | state = currAnimation |> Animation.reverse |> AnimatingTo Reset }
                    , Just model.onPlay
                    )

                AnimatingTo Reset currAnimation ->
                    ( { model | state = currAnimation |> Animation.reverse |> AnimatingTo Play }
                    , Just model.onReset
                    )

        NoOp ->
            ( model, Nothing )


animate : Time -> Model msg -> Model msg
animate dt model =
    case model.state of
        AnimatingTo nextState animation ->
            let
                updatedAnimation =
                    animation |> Animation.run dt
            in
                if updatedAnimation |> Animation.isDone then
                    { model | state = Plain nextState }
                else
                    { model | state = AnimatingTo nextState updatedAnimation }

        _ ->
            model



-- VIEW


view : Model msg -> Svg Msg
view model =
    let
        params =
            case model.state of
                AnimatingTo _ running ->
                    running

                Plain Play ->
                    playToReset

                Plain Reset ->
                    resetToPlay
    in
        [ box, triangle, tail ]
            |> List.map
                (\viewF ->
                    params
                        |> Animation.sample
                        |> viewF model
                )
            |> S.g [ HE.onClick Clicked ]


type alias Triangle a =
    { a
        | alpha : Float
        , r : Float
        , s : Float
        , d : Float
    }


type alias Tail a =
    { a
        | r : Float
        , alpha : Float
        , gamma : Float
    }


playToReset : Animation Params
playToReset =
    Time.second
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


resetToPlay : Animation Params
resetToPlay =
    playToReset
        |> Animation.reverse
        |> Animation.reset


box : a -> b -> Svg msg
box _ _ =
    S.rect
        [ SA.x "-1"
        , SA.y "-1"
        , SA.width "2"
        , SA.height "2"
        , SA.fill "transparent"
        ]
        []


triangle : { b | fillColor : String } -> Triangle a -> Svg msg
triangle { fillColor } { alpha, r, s, d } =
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
                |> Helpers.pointsToString
    in
        S.g [ SA.transform transform ]
            [ S.polygon [ SA.points points, SA.fill fillColor ] [] ]


tail : { b | fillColor : String } -> Tail a -> Svg msg
tail { fillColor } ({ r, alpha, gamma } as tail) =
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
