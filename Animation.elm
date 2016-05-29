-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Animation
    exposing
        ( Animation
        , State(..)
        , run
        , runState
        , sample
        , sampleState
        , isDone
        , interval
        , append
        , map
        , immediately
        )

{-|

@docs Animation, State

# Running
@docs run, runState, isDone

# Sampling and querying
@docs sample, sampleState

# Building
@docs interval, map, immediately, append

-}

import Time exposing (Time)


{-| An animation describes how a value changes over a finite period of time.

-}
type Animation a
    = LastStep (Step a)
    | WithPrefix (Step a) (Animation a)


type alias Step a =
    { now : Time, end : Time, f : Time -> a }


{-| A state is the result of running an animation for some time. An animation
ends eventually so we need a case for it's final state. It can also not be done
after the time we've run it for - then we'll just keep it going next time we get
to.

-}
type State a
    = Done a
    | Continuing (Animation a)


{-| Runs an animation for some time and returns a state.

-}
run : Time -> Animation a -> State a
run t animation =
    if t < 0 then
        Continuing animation
    else
        let
            runStep ({ now, end } as step) wrapStep orElse =
                if now + t < end then
                    wrapStep { step | now = now + t }
                else
                    orElse ()
        in
            case animation of
                LastStep ({ end, f } as step) ->
                    runStep step
                        (Continuing << LastStep)
                        (\_ -> Done <| f end)

                WithPrefix ({ now, end } as step) rest ->
                    runStep step
                        (Continuing << flip WithPrefix rest)
                        (\_ -> run (t - (end - now)) rest)


{-| Like [`run`](#run) but accepts an animation state instead of an animation.

-}
runState : Time -> State a -> State a
runState dt state =
    stateMap (run dt) Done state



-- QUERY


{-| Gets the current value of the animation.

-}
sample : Animation a -> a
sample animation =
    case animation of
        LastStep { now, f } ->
            f now

        WithPrefix { now, f } _ ->
            f now


{-| Like [`sample`](#sample) for animation states instead of animations. If the
state [`isDone`](#isDone), then this is the final state the animation has
reached.

-}
sampleState : State a -> a
sampleState state =
    stateMap sample identity state


{-| True when the animation is over. It can make code clearer when you want to
do things that don't depend on the final/current value.

-}
isDone : State a -> Bool
isDone state =
    stateMap (always False) (always True) state



-- BUILD


{-| Creates an animation that takes no time and assumes the specified value.

-}
immediately : a -> Animation a
immediately value =
    0 |> interval |> map (always value)


{-| Builds an animation spanning from zero to the time specified, and whose
value is the time that's passed since it's start.

For negative values you'll get the zero interval.

-}
interval : Time -> Animation Time
interval t =
    LastStep { now = 0, end = t, f = identity }


{-| Glues two animations together.

-}
append : Animation a -> Animation a -> Animation a
append first second =
    case first of
        LastStep step ->
            WithPrefix step second

        WithPrefix step rest ->
            WithPrefix step (rest `append` second)


{-| Builds an animation with the values transformed by a function.

-}
map : (a -> b) -> Animation a -> Animation b
map g animation =
    let
        mapStep ({ f } as step) =
            { step | f = f >> g }
    in
        case animation of
            LastStep step ->
                LastStep <| mapStep step

            WithPrefix step rest ->
                WithPrefix (mapStep step) (map g rest)



-- HELPERS


stateMap : (Animation a -> b) -> (a -> b) -> State a -> b
stateMap f g state =
    case state of
        Continuing anim ->
            f anim

        Done x ->
            g x
