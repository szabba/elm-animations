module Main exposing (..)

import Html.App as App
import Html.Attributes as HA
import Svg as S exposing (Svg, Attribute)
import Svg.Attributes as SA
import Task
import Time exposing (Time)
import Window
import Ease exposing (Easing)
import Button
import Plot
import Slider


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
    { slider : Slider.Model
    , plot : Plot.Model
    , button : Button.Model Msg
    , size : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Slider.new
            { easing = Ease.inBack
            , time = Time.second
            , width = 0
            , radius = 10
            , fillColor = fillColor
            }
        )
        (Plot.init (3 * Time.second) Ease.inBack)
        (Button.init
            { onPlay = Plot.Start |> PlotMsg
            , onReset = Plot.Reset |> PlotMsg
            , fillColor = fillColor
            }
        )
        (Window.Size 0 0)
    , Task.perform (\_ -> Debug.crash "window has no size?!") Resize Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ model.plot |> Plot.subscriptions |> Sub.map PlotMsg
        , model.button |> Button.subscriptions |> Sub.map ButtonMsg
        , Window.resizes Resize
        ]



-- UPDATE


type Msg
    = Resize Window.Size
    | PlotMsg Plot.Msg
    | ButtonMsg Button.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            { model | size = size } ! []

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
    let
        { width, height } =
            model.size

        transform =
            "translate(" ++ toString (width // 2) ++ " " ++ toString (height // 2) ++ ")"

        buttonTransform =
            {- "translate(0 30) " ++ -}
            " scale(10)"

        plotStyle =
            Plot.Style (width // 2) (height // 2) fillColor 100
    in
        S.svg
            [ SA.version "1.1"
            , SA.baseProfile "full"
            , HA.attribute "xmlns" "http://www.w3.org/2000/svg"
            , SA.width <| toString <| width
            , SA.height <| toString <| height
            , SA.transform transform
            , HA.style [ (,) "display" "block" ]
            ]
            [ Plot.view plotStyle model.plot
            , S.g
                [ SA.transform buttonTransform
                ]
                [ model.button |> Button.view |> App.map ButtonMsg ]
            ]


fillColor : String
fillColor =
    "#DD7700"
