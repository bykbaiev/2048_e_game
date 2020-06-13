module Main exposing (main)

import Browser
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode

type Direction
    = Left
    | Right
    | Up
    | Down
    | Other

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Msg
toDirection string =
  case string of
    "ArrowLeft" ->
        KeyDown Left

    "ArrowRight" ->
        KeyDown Right

    "ArrowUp" ->
        KeyDown Up

    "ArrowDown" ->
        KeyDown Down

    _ ->
        KeyDown Other

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Cell = Maybe Int

type alias Model =
    { direction: String
    , score: Int
    , board: List ( List Cell )
    }

getInitialBoardState : List ( List Cell )
getInitialBoardState =
    [ [Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing]
    ]

init : () -> ( Model, Cmd Msg )
init () =
    (
        { direction = ""
        , score = 0
        , board = getInitialBoardState
        }
        , Cmd.none
    )

-- UPDATE

type Msg =
    KeyDown Direction
    | Reset

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown direction ->
            case direction of
                Up ->
                    ( { model | direction = "Up" }, Cmd.none )

                Right ->
                    ( { model | direction = "Right" }, Cmd.none )

                Down ->
                    ( { model | direction = "Down" }, Cmd.none )

                Left ->
                    ( { model | direction = "Left" }, Cmd.none )

                Other ->
                    ( model, Cmd.none )
        Reset ->
            ( { model | score = 0 }, Cmd.none )

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder

--VIEW

view : Model -> Html Msg
view model =
    div
        []
        [button [ onClick Reset ] [ text "-" ]
        , div [] [ text model.direction ]
        , div [] [ text (String.fromInt model.score) ]
        , button [ onClick Reset ] [ text "+" ]
        ]
