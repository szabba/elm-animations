-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Animation
    exposing
        ( Animation
        , interval
        , append
        , map
        , andMap
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

@docs Animation, interval, immediately, append

# Queries
@docs sample, timeLeft

# Combining
@docs map, andMap

# Running
@docs State, isDone, run
@docs runState, sampleState, timeLeftState


-}

import Time exposing (Time)


{-| An animation describes how a value changes over a finite period of time.

-}
type Animation a
    = Animation { now : Time, end : Time, f : Time -> a }



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
run t ((Animation record) as animation) =
    if t < 0 then
        animation |> Continuing
    else if t < timeLeft animation then
        { record | now = record.now + t }
            |> Animation
            |> Continuing
    else
        Done <| record.f record.end


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
        |> Animation


{-| Glues two animations together.

-}
append : Animation a -> Animation a -> Animation a
append (Animation left) (Animation right) =
    { now = left.now
    , end = left.end + right.end - right.now
    , f =
        \t ->
            if t < left.end then
                left.f t
            else
                right.f <| t - left.end + right.now
    }
        |> Animation


{-| Builds an animation with the values transformed by a function. If you want
to use a function with multiple arguments, you're looking for
[`andMap`](#andMap)

-}
map : (a -> b) -> Animation a -> Animation b
map f (Animation record) =
    { record | f = record.f >> f }
        |> Animation


{-| Applies functions produced by one animation to values produced by another
one. In tandem with [`map`](#map), this allows you to combine several animations
using a multiple-parameter function.

    type alias Position =
        { x : Float, y : Float }

    position : Animation Position
    position =
        Position `Animation.map` x `Animation.andMap` y

    y : Animation Float
    y =
        Animation.interval pi
        |> Animation.map cos

    x : Animation Float
    x = Animation.interval pi


-}
andMap : Animation (a -> b) -> Animation a -> Animation b
andMap (Animation f) (Animation x) =
    let
        ( end, total ) =
            ( f.end `max` x.end
            , f.end - f.now `max` x.end - x.now
            )

        ext =
            extend total >> .f
    in
        { now = 0
        , end = end
        , f = \t -> ext f t <| ext x t
        }
            |> Animation


extend :
    Time
    -> { now : Time, end : Time, f : Time -> a }
    -> { now : Time, end : Time, f : Time -> a }
extend to anim =
    { anim
        | end = anim.now + to
        , f =
            \t ->
                anim.f
                    (if t <= anim.end then
                        t
                     else
                        anim.end
                    )
    }



-- QUERY


{-| Gets the current value of the animation.

-}
sample : Animation a -> a
sample (Animation { now, f }) =
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
timeLeft (Animation { now, end }) =
    end - now


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
