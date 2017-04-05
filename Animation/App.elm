-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Animation.App exposing (program, programWithFlags)

{-| This module constains helpers for quickly constructing a program that runs
an [`Animation`](./Animation#Animation).

@docs program, programWithFlags

-}

import AnimationFrame
import Html exposing (Attribute)
import Html.Attributes as HA
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Task
import Time exposing (Time)
import Window
import Animation exposing (Animation)


{-| Create a
[`Program`](http://package.elm-lang.org/packages/elm-lang/core/latest/Platform#Program)
that runs an animation, possibly looping over it.

-}
program : { init : Animation (Window.Size -> Svg Never), loop : Bool } -> Program Never Model Msg
program args =
    Html.program
        { init = init args.init
        , subscriptions = subscriptions
        , update = update { loop = args.loop }
        , view = view
        }


{-| Same as [`program`](#program), except it accepts extra arguments to be
specified from JS when the module's being initialized. It is what
[`Html.programWithFlags`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#programWithFlags)
is to [`Html.program`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#program).

-}
programWithFlags :
    { init : flags -> Animation (Window.Size -> Svg Never)
    , loop : Bool
    }
    -> Program flags Model Msg
programWithFlags args =
    Html.programWithFlags
        { init = \flags -> init <| args.init flags
        , subscriptions = subscriptions
        , update = update { loop = args.loop }
        , view = view
        }


type alias Model =
    { window : Window.Size
    , animation : Animation (Window.Size -> Svg Never)
    }


init : Animation (Window.Size -> Svg Never) -> ( Model, Cmd Msg )
init animation =
    { window = Window.Size 0 0
    , animation = animation
    }
        ! [ Window.size |> Task.perform Resize ]


type Msg
    = Resize Window.Size
    | Animate Time


subscriptions : Model -> Sub Msg
subscriptions { animation } =
    [ if Animation.isDone animation then
        Sub.none
      else
        AnimationFrame.diffs Animate
    , Window.resizes Resize
    ]
        |> Sub.batch


update : { loop : Bool } -> Msg -> Model -> ( Model, Cmd Msg )
update { loop } msg model =
    case msg |> Debug.log "msg" of
        Animate dt ->
            let
                ( movedAnimation, unusedTime ) =
                    model.animation
                        |> Animation.animate dt
            in
                if unusedTime > 0 && loop then
                    { model
                        | animation =
                            model.animation
                                |> Animation.reset
                    }
                        |> update { loop = loop } (Animate unusedTime)
                else
                    { model | animation = movedAnimation } ! []

        Resize newSize ->
            { model | window = newSize } ! []


view : Model -> Svg Msg
view { window, animation } =
    let
        svg =
            animation
                |> Animation.map (\forSize -> forSize window)
                |> Animation.sample
                |> Html.map never
    in
        fullScreen window [ svg ]


fullScreen : Window.Size -> List (Svg msg) -> Svg msg
fullScreen { width, height } children =
    S.svg
        [ SA.version "1.1"
        , SA.baseProfile "full"
        , HA.attribute "xmlns" "http://www.w3.org/2000/svg"
        , SA.width <| toString <| width
        , SA.height <| toString <| height
        , HA.style [ (,) "display" "block" ]
        ]
        [ S.g
            [ SA.transform <| "translate(" ++ toString (width // 2) ++ " " ++ toString (height // 2) ++ ")"
            ]
            children
        ]
