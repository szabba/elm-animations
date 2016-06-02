module Slider exposing (Model, new, subscriptions, setWidth, Msg(Start, Reset), update, view)

import AnimationFrame
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Time)
import Ease exposing (Easing)
import Animation


-- MODEL


type alias Model =
    { animation : Animation.State Float
    , easing : Easing
    , time : Time
    , width : Int
    , radius : Int
    , fillColor : String
    }


new : { easing : Easing, time : Time, width : Int, radius : Int, fillColor : String } -> Model
new { easing, time, width, radius, fillColor } =
    Model zeroState easing time width radius fillColor


subscriptions : Model -> Sub Msg
subscriptions model =
    if Animation.isDone model.animation then
        Sub.none
    else
        AnimationFrame.diffs Animate


setWidth : Int -> Model -> Model
setWidth newWidth model =
    { model | width = newWidth }



-- UPDATE


type Msg
    = Animate Time
    | Start
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg |> Debug.log "slider msg" of
        Animate dt ->
            { model | animation = Animation.runState dt model.animation }

        Start ->
            { model | animation = animation model.time model.easing }

        Reset ->
            { model | animation = zeroState }



-- VIEW


view : Model -> Svg msg
view model =
    let
        ( leftmost, rightmost ) =
            ( model.width // -2, model.width // 2 )

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
                , SA.strokeWidth "0.5"
                ]
                []
            , S.circle
                [ SA.cx <| toString position
                , SA.cy "0"
                , SA.r <| toString <| model.radius
                , SA.fill model.fillColor
                ]
                []
            ]


animation : Time -> Easing -> Animation.State Float
animation t easing =
    t
        |> Animation.interval
        |> Animation.map (flip (/) t >> easing)
        |> Animation.Continuing


zeroState : Animation.State Float
zeroState =
    Animation.Done 0
