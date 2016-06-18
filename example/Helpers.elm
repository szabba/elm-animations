module Helpers exposing (..)

import Time exposing (Time)
import Animation exposing (Animation)


freezeEndFor : Time -> Animation a -> Animation a
freezeEndFor t animation =
    animation |> Animation.continue t always
