module Main exposing (..)

import AnimationFrame
import Html.App as App
import Html.Attributes as HA
import List.Extra as List
import Svg as S exposing (Svg, Attribute)
import Svg.Attributes as SA
import Task
import Time exposing (Time)
import Window
import Ease exposing (Easing)
import Button
import Plot


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
    { plot : Plot.Model
    , button : Button.Model Msg
    , size : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Plot.init (3 * Time.second)
            Ease.inBounce
        )
        (Button.init
            { onPlay = Plot.Start |> PlotMsg
            , onReset = Plot.Reset |> PlotMsg
            , fillColor = fillColor
            }
        )
        (Window.Size 0 0)
    , Window.size
        |> Task.perform (\_ -> Debug.crash "window has no size?!") Resize
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if
            Plot.needsAnimating model.plot
                || Button.needsAnimating model.button
          then
            AnimationFrame.diffs Animate
          else
            Sub.none
        , Window.resizes Resize
        ]



-- UPDATE


type Msg
    = Resize Window.Size
    | PlotMsg Plot.Msg
    | ButtonMsg Button.Msg
    | Animate Time
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            { model | size = size } ! []

        Animate dt ->
            { model
                | plot = model.plot |> Plot.update (Plot.Animate dt)
                , button = model.button |> Button.animate dt
            }
                ! []

        PlotMsg msg ->
            { model | plot = model.plot |> Plot.update msg } ! []

        ButtonMsg msg ->
            let
                ( newButton, event ) =
                    model.button |> Button.update msg
            in
                { model | button = newButton }
                    |> update (event |> Maybe.withDefault NoOp)

        NoOp ->
            model ! []



-- VIEW


view : Model -> Svg Msg
view model =
    [ plot
    , playResetButton
    ]
        |> List.map ((|>) model)
        |> fullScreen model.size []


fullScreen : Window.Size -> List (Attribute msg) -> List (Svg msg) -> Svg msg
fullScreen { width, height } attrs =
    [ SA.version "1.1"
    , SA.baseProfile "full"
    , HA.attribute "xmlns" "http://www.w3.org/2000/svg"
    , SA.width <| toString <| width
    , SA.height <| toString <| height
    , SA.transform <| "translate(" ++ toString (width // 2) ++ " " ++ toString (height // 2) ++ ")"
    , HA.style [ (,) "display" "block" ]
    ]
        |> (++) attrs
        |> S.svg


plot : Model -> Svg msg
plot model =
    model.plot
        |> Plot.view
            { width = model.size.width // 2
            , height = model.size.height // 2
            , color = fillColor
            , density = 250
            }


playResetButton : Model -> Svg Msg
playResetButton model =
    model.button
        |> Button.view
        |> App.map ButtonMsg
        |> List.singleton
        |> S.g
            [ SA.transform <| "translate(" ++ toString (model.size.width // -4 - 30) ++ ") scale(10)"
            ]


fillColor : String
fillColor =
    "#DD7700"
