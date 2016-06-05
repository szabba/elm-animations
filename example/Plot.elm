module Plot exposing (Model, init, subscriptions, Msg(Start, Reset), update, Style, view)

import AnimationFrame
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Time)
import Ease exposing (Easing)
import Tuple2
import Animation exposing (Animation)
import Helpers exposing (pointsToString)


-- MODEL


type alias Model =
    { progress : Animation Float
    , time : Time
    , easing : Easing
    }


init : Time -> Easing -> Model
init =
    Model <| Animation.immediately 0


subscriptions : Model -> Sub Msg
subscriptions { progress } =
    if Animation.isDone progress then
        Sub.none
    else
        AnimationFrame.diffs Animate



-- UPDATE


type Msg
    = Start
    | Reset
    | Animate Time


update : Msg -> Model -> Model
update msg model =
    case msg of
        Start ->
            { model
                | progress =
                    model.time
                        |> Animation.interval
                        |> Animation.map model.easing
            }

        Reset ->
            { model | progress = Animation.immediately 0 }

        Animate dt ->
            { model
                | progress =
                    model.progress
                        |> Animation.run dt
            }



-- VIEW


type alias Style =
    { width : Int
    , height : Int
    , color : String
    , density : Float
    }


view : Style -> Model -> Svg msg
view style model =
    let
        allData =
            points style { model | progress = Animation.immediately 1 }

        data =
            points style model
    in
        S.g
            [ SA.transform <| "translate(" ++ (toString <| style.width // -2) ++ " " ++ (toString <| style.height // 2) ++ ")"
            ]
            [ plot { style | color = "#DDDDDD" } allData
            , plot style data
            , axis { x = 0, y = style.height }
            , axis { x = style.width, y = 0 }
            ]


axis : { x : Int, y : Int } -> Svg msg
axis { x, y } =
    S.line
        [ SA.x1 "0"
        , SA.y1 "0"
        , SA.x2 <| toString x
        , SA.y2 <| toString -y
        , SA.stroke "#000000"
        , SA.strokeWidth "1"
        ]
        []


plot : Style -> List ( Float, Float ) -> Svg msg
plot { color, width, height } data =
    let
        scaleTo =
            toFloat >> (*)
    in
        S.polyline
            [ data
                |> List.map (Tuple2.mapEach (scaleTo width) (scaleTo height))
                |> pointsToString
                |> SA.points
            , SA.stroke color
            , SA.strokeWidth "1"
            , SA.fillOpacity "0"
            ]
            []


points : Style -> Model -> List ( Float, Float )
points { density } { easing, progress } =
    let
        step =
            1 / density

        xs =
            density
                |> round
                |> flip List.repeat step
                |> List.scanl (+) 0
                |> List.filter ((>=) <| Animation.sample progress)
                |> Debug.log "xs"
    in
        xs
            |> List.map (\x -> ( x, negate <| easing x ))
