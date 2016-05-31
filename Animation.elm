-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Animation
    exposing
        ( Animation
        , interval
        , append
        , map
        , immediately
        , sample
        , timeLeft
        , State(..)
        , isDone
        , run
        , runState
        , sampleState
        , timeLeftState
        )

{-|

@docs Animation, interval, map, immediately, append

# Queries
@docs sample, timeLeft

# Running
@docs State, isDone, run
@docs runState, sampleState, timeLeftState


-}

import Time exposing (Time)
import Animation.Step as Step exposing (Step)


{-| An animation describes how a value changes over a finite period of time.

-}
type Animation a
    = Animation (Step a) (List (Step a))



-- RUNNING


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
run t ((Animation step rest) as animation) =
    if t < 0 then
        animation |> Continuing
    else if t < step.end - step.now then
        { step | now = step.now + t }
            |> flip Animation rest
            |> Continuing
    else
        animation
            |> dropStep
            |> Maybe.map (run <| t - Step.timeLeft step)
            |> Maybe.withDefault (Done <| sample animation)


{-| Like [`run`](#run) but accepts an animation state instead of an animation.

-}
runState : Time -> State a -> State a
runState dt state =
    stateMap (run dt) Done state



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
    { now = 0, end = t, f = identity }
        |> flip Animation []


{-| Glues two animations together.

-}
append : Animation a -> Animation a -> Animation a
append (Animation step rest) (Animation rest' rest'') =
    Animation step <| rest ++ [ rest' ] ++ rest''


{-| Builds an animation with the values transformed by a function.

-}
map : (a -> b) -> Animation a -> Animation b
map f (Animation step rest) =
    Animation (Step.map f step) <| List.map (Step.map f) rest



-- QUERY


{-| Gets the current value of the animation.

-}
sample : Animation a -> a
sample (Animation { now, f } _) =
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


{-| The time left until the animation ends.

-}
timeLeft : Animation a -> Time
timeLeft (Animation step rest) =
    rest
        |> List.map Step.timeLeft
        |> List.sum
        |> (+) (Step.timeLeft step)


{-| Like [`time`](#time), but for animation states instead of animations.

-}
timeLeftState : State a -> Time
timeLeftState state =
    stateMap timeLeft (always 0) state



-- HELPERS


stateMap : (Animation a -> b) -> (a -> b) -> State a -> b
stateMap f g state =
    case state of
        Continuing anim ->
            f anim

        Done x ->
            g x


dropStep : Animation a -> Maybe (Animation a)
dropStep (Animation _ rest) =
    case rest of
        step :: rest ->
            Just <| Animation step rest

        [] ->
            Nothing
