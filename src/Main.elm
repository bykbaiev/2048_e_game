module Main exposing (main)

import Browser
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
-- import Html.Styled.Events exposing (onClick)
import Css exposing (..)
import Css.Transitions
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Random

cellSize = 107

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
    Maybe
        { x : Int
        , y : Int
        , value: Int
        }

type alias GameGrid = List ( List ( Cell ) )

type alias Model =
    { bestScore: Int
    , size: Int
    , grid: GameGrid
    , score : Int
    , won : Bool
    }

emptyGrid : Int -> GameGrid
emptyGrid size =
    List.repeat size <| List.repeat size Nothing

init : () -> ( Model, Cmd Msg )
init () =
    (
        { grid = emptyGrid 4
        , score = 0
        , won = False
        , bestScore = 0
        , size = 4 -- can be configurable one day
        }
        , Cmd.none
        -- Random.list ( 2 * 3 ) ( Random.int 0 3 )
        --     -- |> valueToCell
        --     |> Random.generate InitialState

        -- Random.generate InitialState ( Random.int 0 4 ) --, ( Random.int 0 4 ), ( Random.int 1 2 ) )
    )

-- UPDATE

type Msg =
    KeyDown Direction
    | Reset
    -- | InitialState List Int -- Cell Cell -- ( Int, Int, Int ) -- (Int Int Int)

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

        -- InitialState v -> --p1 p2 -> -- (x, y, value) -> -- (x1, y1, value1) ->
            -- ( model, Cmd.none )

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder

--VIEW

view : Model -> Html Msg
view model =
    div
        []
        [ viewHeader model.score model.bestScore
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
        [ viewGrid model.size
        , viewTiles model.size model.grid
        ]

viewGrid : Int -> Html Msg
viewGrid size =
    div [] <|
        List.repeat size ( viewGridRow size )

viewGridRow : Int -> Html Msg
viewGridRow size =
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , marginBottom <| px 15
            ]
        ]
        <|
            List.repeat size ( viewGridCell () )

viewGridCell : () -> Html Msg
viewGridCell _ =
    div
        [ css
            [ width <| px 106.25
            , height <| px 106.25
            , borderRadius <| px 3
            , backgroundColor <| rgba 238 228 218 0.35
            ]
        ] []

viewTiles : Int -> GameGrid -> Html Msg
viewTiles size grid =
    div
        []
        (
            grid
                |> List.foldr (++) []
                |> List.filter
                    (\value ->
                        case value of
                            Just _ ->
                                True

                            Nothing ->
                                False
                    )
                |> List.map viewTile
        )

viewTile : Cell -> Html Msg
viewTile cell =
    case cell of
        Just cellValue ->
            let
                x = cellValue.x
                y = cellValue.y
                value = cellValue.value
            in
                div
                    [ css
                        [ position absolute
                        , width <| px cellSize
                        , height <| px cellSize
                        , lineHeight <| px cellSize
                        , transform <| translate2 ( px ( toFloat <|  x * cellSize ) ) ( px ( toFloat <| y * cellSize ) )
                        , Css.Transitions.transition
                            [ Css.Transitions.transform3 100 0 Css.Transitions.easeInOut ]
                        ]
                    ]
                    [ text <| String.fromInt value ]

        Nothing ->
            text ""
