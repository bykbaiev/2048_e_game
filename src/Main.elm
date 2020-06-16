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
    { x : Int
    , y : Int
    , value: Int
    }

type alias GameGrid = List ( List ( Maybe Cell ) )

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

randomPoint : Random.Generator Cell
randomPoint =
    Random.map3
        (\x y value -> { x = x, y = y, value = if value < 0.9 then 2 else 4 })
        ( Random.int 0 3 )
        ( Random.int 0 3 )
        ( Random.float 0 1 )

init : () -> ( Model, Cmd Msg )
init () =
    (
        { grid = emptyGrid 4
        , score = 0
        , won = False
        , bestScore = 0
        , size = 4 -- can be configurable one day
        }
        , Random.generate InitialState ( Random.list 2 randomPoint )
    )

-- UPDATE

type Msg =
    KeyDown Direction
    | Reset
    | InitialState ( List ( Cell ) )

replaceCell : List ( Cell ) -> Int -> Int -> Maybe Cell -> Maybe Cell
replaceCell cells rowIndex columnIndex oldCell =
    let
        newCell = List.head <| List.filter (\c -> c.x == columnIndex && c.y == rowIndex) cells
    in
        case newCell of
            Just _ ->
                newCell

            Nothing ->
                oldCell

setCells : List ( Cell ) -> ( GameGrid -> GameGrid )
setCells cells =
    List.indexedMap
        (\rowIndex row ->
            List.indexedMap (\columnIndex cell -> replaceCell cells rowIndex columnIndex cell ) row
        )

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

        InitialState cells ->
            ( { model | grid = setCells cells model.grid }, Cmd.none )

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
            , color <| hex "776e65"
            , fontFamilies [ "Clear Sans", "Helvetica Neue", "Arial", "sans-serif" ]
            ]
        ]
        [ viewGrid model.size
        , viewTiles model.size model.grid
        ]

viewGrid : Int -> Html Msg
viewGrid size =
    div
        [ css
            [ position absolute
            , zIndex <| int 1
            ]
        ]
        <|
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
            [ marginRight <| px 15
            , width <| px 106.25
            , height <| px 106.25
            , borderRadius <| px 3
            , backgroundColor <| rgba 238 228 218 0.35
            ]
        ] []

viewTiles : Int -> GameGrid -> Html Msg
viewTiles size grid =
    div
        [ css
            [ position absolute
            , zIndex <| int 2
            ]
        ]
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

viewTile : Maybe Cell -> Html Msg
viewTile cell =
    case cell of
        Just cellValue ->
            let
                wholeCellSize = cellSize + 15
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
                        , transform <| translate2 ( px ( toFloat <|  x * wholeCellSize ) ) ( px ( toFloat <| y * wholeCellSize ) )
                        , Css.Transitions.transition
                            [ Css.Transitions.transform3 100 0 Css.Transitions.easeInOut ]
                        ]
                    ]
                    [ div
                        [ css
                            [ borderRadius <| px 3
                            , backgroundColor <| hex "EEE4DA"
                            , textAlign center
                            , fontWeight bold
                            , fontSize <| px 55
                            , zIndex <| int 10
                            ]
                        ]
                        [ text <| String.fromInt value ]
                    ]

        Nothing ->
            text ""
