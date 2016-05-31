-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Animation.Step exposing (..)

import Time exposing (Time)


type alias Step a =
    { now : Time, end : Time, f : Time -> a }


map : (a -> b) -> Step a -> Step b
map g ({ f } as step) =
    { step | f = f >> g }


timeLeft : Step a -> Time
timeLeft { now, end } =
    end - now
