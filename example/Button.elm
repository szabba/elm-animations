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


type alias Model =
    { animation : Animation.State Params
    , state : State
    }


type State
    = Play
    | Reset
    | AnimatingTo State


init : Model
init =
    { animation = Animation.Done <| Animation.sample animation
    , state = Play
    }


type alias Params =
    { alpha : Float
    , gamma : Float
    , r : Float
    , s : Float
    , d : Float
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    if Animation.isDone model.animation then
        Sub.none
    else
        AnimationFrame.diffs Animate



-- UPDATE


type Msg
    = Animate Time
    | Clicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        Animate dt ->
            model |> animate (dt |> Debug.log "animating button")

        Clicked ->
            model |> onClick


animate : Time -> Model -> Model
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
        { animation = animation
        , state = state
        }


onClick : Model -> Model
onClick model =
    case model.state of
        AnimatingTo _ ->
            model

        Play ->
            { animation = Animation.Continuing animation
            , state = AnimatingTo Reset
            }

        Reset ->
            { animation = Animation.Continuing reverseAnimation
            , state = AnimatingTo Play
            }



-- VIEW


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


view : Model -> Svg Msg
view model =
    [ viewTriangle, viewTail ]
        |> List.map ((|>) (Animation.sampleState model.animation))
        |> S.g [ HE.onClick Clicked ]


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


viewTriangle : Triangle a -> Svg msg
viewTriangle { alpha, r, s, d } =
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
            [ S.polygon [ SA.points points ] [] ]


viewTail : Tail a -> Svg msg
viewTail ({ r, alpha, gamma } as tail) =
    S.g
        [ SA.transform <| "rotate(" ++ (toString <| (+) 89 << toDegrees <| -alpha) ++ ")"
        ]
        [ S.path
            [ SA.d <| tailPath tail
            , SA.stroke "#000000"
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
