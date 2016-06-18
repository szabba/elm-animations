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
import Html.App as App
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
program : { init : Animation (Window.Size -> Svg msg), loop : Bool } -> Program Never
program { init, loop } =
    programWithFlags
        { init = always init
        , loop = loop
        }


{-| Same as [`program`](#program), except it accepts extra arguments to be
specified from JS when the module's being initialized. It is what
[`Html.App.programWithFlags`](http://package.elm-lang.org/packages/elm-lang/html/1.0.0/Html-App#programWithFlags)
is to [`Html.App.program`](http://package.elm-lang.org/packages/elm-lang/html/1.0.0/Html-App#program).

-}
programWithFlags :
    { init : flags -> Animation (Window.Size -> Svg msg)
    , loop : Bool
    }
    -> Program flags
programWithFlags args =
    App.programWithFlags
        { init = \flags -> init <| args.init flags
        , subscriptions = subscriptions
        , update = update { loop = args.loop }
        , view = view
        }


type alias Model msg =
    { window : Window.Size
    , animation : Animation (Window.Size -> Svg msg)
    }


init : Animation (Window.Size -> Svg msg) -> ( Model msg, Cmd Msg )
init animation =
    { window = Window.Size 0 0
    , animation = animation
    }
        ! [ Window.size |> Task.perform (always NoOp) Resize ]


type Msg
    = Resize Window.Size
    | Animate Time
    | NoOp


subscriptions : Model msg -> Sub Msg
subscriptions { animation } =
    [ if Animation.isDone animation then
        Sub.none
      else
        AnimationFrame.diffs Animate
    , Window.resizes Resize
    ]
        |> Sub.batch


update : { loop : Bool } -> Msg -> Model msg -> ( Model msg, Cmd Msg )
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

        NoOp ->
            model ! []


view : Model msg -> Svg Msg
view { window, animation } =
    let
        svg =
            animation
                |> Animation.map (\forSize -> forSize window)
                |> Animation.sample
                |> App.map (always NoOp)
    in
        fullScreen window [] [ svg ]


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
