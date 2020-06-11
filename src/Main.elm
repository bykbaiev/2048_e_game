module Main exposing (main)

import Browser
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Browser.Events exposing (onKeyDown)
import Json.Decode as D

type Direction
    = Left
    | Right
    | Up
    | Down
    | Other

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

-- MODEL

type alias Cell = Maybe Int

type alias Model =
    { keyCode: Maybe Int
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

getKeyCode : Maybe Int -> String
getKeyCode keyCode =
    case keyCode of
        Just value ->
            String.fromInt value

        Nothing ->
            ""

init : () -> ( Model, Cmd Msg )
init () =
    (
        { keyCode = Nothing
        , score = 0
        , board = getInitialBoardState
        }
        , Cmd.none
    )

-- UPDATE

type Msg =
    KeyDown String
    | Reset

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown keyCode ->
            ( { model | keyCode = String.toInt keyCode }, Cmd.none )
        Reset ->
            ( { model | score = 0 }, Cmd.none )

--VIEW

-- button [ onClick Reset ] [ text "-" ]
-- , div [] [ text (String.fromInt model.score) ]
-- , div [] [ text (getKeyCode model.keyCode) ]
-- , button [ onClick Reset ] [ text "+" ]

view : Model -> Html Msg
view model =
    div
        [ onClick <| KeyDown "asdf" ]
        [ text "Hello world" ]

-- keyDecoder : Decode.Decoder Direction
-- keyDecoder = Decode.map toDirection (Decode.field "key" Decode.string)

-- toDirection : String -> Direction
-- toDirection key =
--     case key of
--         "ArrowLeft" ->
--             Left

--         "ArrowRight" ->
--             Right

--         "ArrowUp" ->
--             Up

--         "ArrowDown" ->
--             Down

--         _ -> Other
