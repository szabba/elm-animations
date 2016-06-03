-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Animation
    exposing
        ( Animation
        , run
        , animate
        , immediately
        , interval
        , time
        , sample
        , timeLeft
        , isDone
        , reverse
        , reset
        , append
        , continue
        , map
        , andMap
        )

{-|

@docs Animation, run, animate

# Building
@docs immediately, interval, time

# Queries
@docs sample, timeLeft, isDone

# Transforms
@docs reverse, reset

# Combining
@docs append, continue, map, andMap

-}

import Time exposing (Time)


{-| An animation describes how a value changes over a finite period of time.

-}
type Animation a
    = Animation { now : Time, end : Time, f : Time -> a }



-- RUNNING


{-| Runs an animation for some time. If you need to know when the animation
"ends early", leaving time to spare, consider using [`animate`](#animate)
instead.

-}
run : Time -> Animation a -> Animation a
run t ((Animation { now, end, f }) as animation) =
    if t < 0 then
        animation
    else if t < timeLeft animation then
        { now = now + t, end = end, f = f }
            |> Animation
    else
        { now = end, end = end, f = f }
            |> Animation


{-| Like [`run`](#run), but it also includes the amount of time *not* used up by
the animation.

-}
animate : Time -> Animation a -> ( Animation a, Time )
animate t animation =
    if t < timeLeft animation then
        ( run t animation, 0 )
    else
        ( run t animation, t - timeLeft animation )



-- BUILD


{-| Creates an animation that takes no time and assumes the specified value.

-}
immediately : a -> Animation a
immediately value =
    0 |> interval |> map (always value)


{-| Runs linearily from 0 to 1 over the time specified.

For negative values you'll get the zero interval.

-}
interval : Time -> Animation Float
interval t =
    if t < 0 then
        immediately 0
    else
        { now = 0, end = t, f = flip (/) t }
            |> Animation


{-| Runs linearily from 0 to the time specified over the time specified.

For negative values you'll get the zero interval.

-}
time : Time -> Animation Time
time t =
    t |> interval |> map ((*) t)


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


{-| Reverses the arrow of time in the animation.

-}
reverse : Animation a -> Animation a
reverse (Animation { now, end, f }) =
    { now = end - now
    , end = end
    , f = \t -> f (end - t)
    }
        |> Animation


{-| Moves the animation back to it's starting point - it's state before it
consumed any time.

-}
reset : Animation a -> Animation a
reset (Animation { end, f }) =
    { now = 0
    , end = end
    , f = f
    }
        |> Animation


{-| Glues on a new period of time to an animation.

    2
        |> Animation.immediately
        |> Animation.continue (2 * Time.second) (\x t -> x + 2 * t)

-}
continue : Time -> (a -> Float -> a) -> Animation a -> Animation a
continue t startWith prefix =
    let
        suffix =
            t
                |> interval
                |> map
                    (prefix
                        |> run (timeLeft prefix)
                        |> sample
                        |> startWith
                    )
    in
        prefix `append` suffix



-- QUERY


{-| Gets the current value of the animation.

-}
sample : Animation a -> a
sample (Animation { now, f }) =
    f now


{-| True when the animation is over.

-}
isDone : Animation a -> Bool
isDone (Animation { now, end }) =
    now == end


{-| The time left until the animation ends.

-}
timeLeft : Animation a -> Time
timeLeft (Animation { now, end }) =
    end - now
