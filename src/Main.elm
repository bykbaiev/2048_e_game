module Main exposing (main)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Css exposing (..)
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
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Cell =
    { value: Int
    , position:
        { x: Int
        , y: Int
        }
    }
type alias GameState =
    { grid:
        { size: Int
        , cells: List ( List ( Maybe Cell ) )
        }
    , score: Int
    , won: Bool
    }

type alias Model =
    { bestScore: Int
    , gameState: GameState
    }

init : () -> ( Model, Cmd Msg )
init () =
    (
        { gameState =
            { grid =
                { size = 4 -- can be configurable one day
                , cells = []
                }
            , score = 0
            , won = False
            }
        , bestScore = 0
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
                    ( model, Cmd.none )

                Right ->
                    ( model, Cmd.none )

                Down ->
                    ( model, Cmd.none )

                Left ->
                    ( model, Cmd.none )

                Other ->
                    ( model, Cmd.none )
        Reset ->
            ( model, Cmd.none )

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder

--VIEW

view : Model -> Html Msg
view model =
    div
        []
        [ viewHeader model.gameState.score model.bestScore
        , viewGameContainer model
        ]

viewHeader : Int -> Int -> Html Msg
viewHeader score bestScore =
    div []
        [ text "2048"
        , text <| String.fromInt score
        , text <| String.fromInt bestScore
        ]

viewGameContainer : Model -> Html Msg
viewGameContainer model =
    div
        [ css
            [ position relative
            , padding <| px 15
            , margin auto
            , marginTop <| px 40
            , backgroundColor <| hex "#BBADA0"
            , borderRadius <| px 6
            , width <| px 500
            , height <| px 500
            , boxSizing borderBox
            ]
        ]
        [ viewGrid model.gameState.grid.size
        ]

viewGrid : Int -> Html Msg
viewGrid size =
    div [] <|
        List.map ( viewGridRow size ) ( List.range 1 size )

viewGridRow : Int -> Int -> Html Msg
viewGridRow size _ =
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , marginBottom <| px 15
            ]
        ]
        <|
            List.map viewGridCell ( List.range 1 size )

viewGridCell : Int -> Html Msg
viewGridCell _ =
    div
        [ css
            [ width <| px 106.25
            , height <| px 106.25
            , borderRadius <| px 3
            , backgroundColor <| rgba 238 228 218 0.35
            ]
        ] []

