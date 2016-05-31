module Main exposing (..)

import Html.App as App
import Html.Attributes as HA
import Html.Events as HE
import Svg as S exposing (Svg, Attribute)
import Svg.Attributes as SA
import Task
import Time exposing (Time)
import AnimationFrame
import Window
import Ease exposing (Easing)
import Animation exposing (Animation)
import Button
import Helpers


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { easing : Easing
    , animation : Animation.State Float
    , button : Animation.State (Svg Msg)
    , time : Time
    , size : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    ( Model Ease.inBack
        (Animation.Done 0)
        (Animation.Done <| Animation.sample Button.triangleAnimation)
        Time.second
        (Window.Size 0 0)
    , Task.perform (\_ -> Debug.crash "window has no size?!") Resize Window.size
    )



-- UPDATE


type Msg
    = Resize Window.Size
    | Animate Time
    | Start
    | Reset
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            { model | size = size } ! []

        Animate dt ->
            { model
                | animation = Animation.runState dt <| model.animation
                , button = Animation.runState dt <| Debug.log "triangle" model.button
            }
                ! []

        Start ->
            { model
                | animation =
                    Animation.interval model.time
                        |> Animation.map (flip (/) model.time >> model.easing)
                        |> Animation.Continuing
                , button =
                    Button.triangleAnimation
                        |> Animation.Continuing
            }
                ! []

        Reset ->
            { model | animation = Animation.Done 0 } ! []

        NoOp ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if Animation.isDone model.animation && Animation.isDone model.button then
            Sub.none
          else
            AnimationFrame.diffs Animate
        , Window.resizes Resize
        ]



-- VIEW


view : Model -> Svg Msg
view model =
    let
        { width, height } =
            model.size

        transform =
            "translate(" ++ toString (width // 2) ++ " " ++ toString (height // 2) ++ ")"

        buttonTransform =
            "translate(0 " ++ toString (buttonShift model.size) ++ ") scale(10)"
    in
        S.svg
            [ SA.version "1.1"
            , SA.baseProfile "full"
            , SA.width <| toString <| width
            , SA.height <| toString <| height
            , HA.attribute "xmlns" "http://www.w3.org/2000/svg"
            , SA.transform transform
            , HA.style [ (,) "display" "block" ]
            ]
            [ slider model
            , S.g
                [ SA.transform buttonTransform
                , HE.onClick Start
                ]
                [ otherButton model ]
            ]


slider : Model -> Svg Msg
slider model =
    let
        width =
            sliderWidth model.size

        ( leftmost, rightmost ) =
            ( width // -2, width // 2 )

        progress =
            Animation.sampleState model.animation

        position =
            leftmost + round (progress * toFloat (rightmost - leftmost))
    in
        S.g []
            [ S.line
                [ SA.x1 <| toString leftmost
                , SA.y1 "0"
                , SA.x2 <| toString rightmost
                , SA.y2 "0"
                , SA.stroke "#000000"
                , SA.strokeWidth "1"
                ]
                []
            , S.circle
                [ SA.cx <| toString position
                , SA.cy "0"
                , SA.r <| toString <| radius model.size
                , fill
                ]
                []
            ]


otherButton : Model -> Svg Msg
otherButton model =
    model.button
        |> Animation.sampleState


button : Model -> Svg Msg
button model =
    let
        ( low, high ) =
            ( negate <| radius model.size, radius model.size )

        points =
            [ (,) low low, (,) high 0, (,) low high ]
                |> Helpers.pointsToString
    in
        S.polygon
            [ HE.onClick
                <| if Animation.isDone model.animation then
                    Start
                   else
                    NoOp
            , SA.points points
            , fill
            ]
            []


buttonShift : Window.Size -> Int
buttonShift size =
    3 * radius size


sliderWidth : Window.Size -> Int
sliderWidth { width } =
    width // 2


radius : Window.Size -> Int
radius { width, height } =
    width `min` height |> flip (//) 40


fill : Attribute Msg
fill =
    SA.fill "#DD0000"
