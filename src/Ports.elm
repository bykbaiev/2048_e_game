port module Ports exposing (..)

import Json.Encode exposing (Value)


port storeGameState : Value -> Cmd msg
