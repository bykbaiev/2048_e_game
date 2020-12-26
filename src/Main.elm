module Main exposing (main)

import Browser
import Game as G
import Html.Styled exposing (toUnstyled)



-- MAIN


main : Program () G.Model G.Msg
main =
    Browser.element
        { init = G.init
        , view = toUnstyled << G.view
        , update = G.update
        , subscriptions = G.subscriptions
        }
