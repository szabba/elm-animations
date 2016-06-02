module Button exposing (Model, init, subscriptions, Msg, update, view)

import Html.Events as HE
import String
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Time)
import AnimationFrame
import Animation exposing (Animation)
import Helpers exposing (toDegrees)


-- MODEL


type alias Model msg =
    { animation : Animation.State Params
    , state : State
    , onPlay : msg
    , onReset : msg
    , fillColor : String
    }


type State
    = SendPlayNext
    | SendResetNext
    | AnimatingTo State


init : { fillColor : String, onPlay : msg, onReset : msg } -> Model msg
init { fillColor, onPlay, onReset } =
    { animation = Animation.Done <| Animation.sample animation
    , state = SendPlayNext
    , onPlay = onPlay
    , onReset = onReset
    , fillColor = fillColor
    }


type alias Params =
    { alpha : Float
    , gamma : Float
    , r : Float
    , s : Float
    , d : Float
    }


subscriptions : Model msg -> Sub Msg
subscriptions model =
    if Animation.isDone model.animation then
        Sub.none
    else
        AnimationFrame.diffs Animate



-- UPDATE


type Msg
    = Animate Time
    | Clicked
    | NoOp


update : Msg -> Model msg -> ( Model msg, Maybe msg )
update msg model =
    case msg of
        Animate dt ->
            ( model |> animate dt, Nothing )

        Clicked ->
            model |> onClick

        NoOp ->
            ( model, Nothing )


animate : Time -> Model msg -> Model msg
animate dt model =
    let
        animation =
            model.animation |> Animation.runState dt

        state =
            if Animation.isDone animation then
                case model.state of
                    AnimatingTo nextState ->
                        nextState

                    _ ->
                        model.state
            else
                model.state
    in
        { model
            | animation = animation
            , state = state
        }


onClick : Model msg -> ( Model msg, Maybe msg )
onClick model =
    case model.state of
        AnimatingTo _ ->
            ( model, Nothing )

        SendPlayNext ->
            ( { model
                | animation = Animation.Continuing animation
                , state = AnimatingTo SendResetNext
              }
            , Just model.onPlay
            )

        SendResetNext ->
            ( { model
                | animation = Animation.Continuing reverseAnimation
                , state = AnimatingTo SendPlayNext
              }
            , Just model.onReset
            )



-- VIEW


view : Model msg -> Svg Msg
view model =
    let
        msg =
            if Animation.isDone model.animation then
                Clicked
            else
                NoOp

        params =
            Animation.sampleState model.animation
    in
        [ viewTriangle, viewTail ]
            |> List.map (\viewF -> viewF model params)
            |> S.g [ HE.onClick msg ]


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


animation : Animation Params
animation =
    let
        dt =
            Time.second
    in
        dt
            |> Animation.interval
            |> Animation.map
                (\t ->
                    { alpha = t / dt * (pi - pi / 6 + pi / 2)
                    , gamma = t / dt * (8 / 5 * pi)
                    , r = t / dt
                    , d = 0.75 * (-1 + t / dt)
                    , s = -0.5 * t / dt + 1
                    }
                )


reverseAnimation : Animation Params
reverseAnimation =
    let
        dt =
            Time.second
    in
        dt
            |> Animation.interval
            |> Animation.map
                (\t ->
                    let
                        phase =
                            (1 - t / dt)
                    in
                        { alpha = phase * (pi - pi / 6 + pi / 2)
                        , gamma = phase * (8 / 5 * pi)
                        , r = phase
                        , d = 0.75 * (-1 + phase)
                        , s = -0.5 * phase + 1
                        }
                )


viewTriangle : { b | fillColor : String } -> Triangle a -> Svg msg
viewTriangle { fillColor } { alpha, r, s, d } =
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


viewTail : { b | fillColor : String } -> Tail a -> Svg msg
viewTail { fillColor } ({ r, alpha, gamma } as tail) =
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
