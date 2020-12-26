module Game exposing (Model, Msg, init, subscriptions, update, view)

import Array
import Browser
import Browser.Events exposing (onKeyDown)
import Css exposing (..)
import Css.Media exposing (Interlace)
import Css.Transitions
import Html.Attributes exposing (value)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Json.Decode as D exposing (Decoder)
import Random
import RandomCell exposing (Cell, randomCells)



-- TYPES


type alias Row =
    List (Maybe Int)


type alias GameGrid =
    List Row


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


type Model
    = Internals
        { bestScore : Int
        , size : Int
        , grid : GameGrid
        , score : Int
        , won : Bool
        , failed : Bool
        , targetScore : Int
        }



-- MODEL


gridSize : Int
gridSize =
    4


initGrid : Int -> GameGrid
initGrid size =
    List.repeat size Nothing
        |> List.repeat size


init : () -> ( Model, Cmd Msg )
init () =
    let
        grid =
            initGrid gridSize
    in
    ( Internals
        { bestScore = 0
        , size = gridSize
        , grid = grid
        , score = 0
        , won = False
        , failed = False
        , targetScore = 2048
        }
    , Random.generate GotNewCells (randomCells 2 (availableCells grid))
    )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder



-- UPDATE


type Msg
    = KeyDown Direction
    | Reset
    | GotNewCells (List Cell)
    | Won
    | Failed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Internals model) =
    case msg of
        KeyDown direction ->
            ( Internals model, Cmd.none )

        Reset ->
            init ()

        GotNewCells cells ->
            let
                grid =
                    withCells cells model.grid

                won =
                    isWon model.targetScore grid

                failed =
                    not won && isFailed model.targetScore grid
            in
            ( Internals
                { model
                    | grid = grid
                    , won = won
                    , failed = failed
                }
            , Cmd.none
            )

        Won ->
            ( Internals model, Cmd.none )

        Failed ->
            ( Internals model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view (Internals model) =
    div
        []
        [ viewHeader model.score model.bestScore
        , viewGameContainer <| Internals model
        , viewFooter model.grid
        ]


viewHeader : Int -> Int -> Html Msg
viewHeader score bestScore =
    div []
        [ text "2048"
        , text <| String.fromInt score
        , text <| String.fromInt bestScore
        ]


viewGameContainer : Model -> Html Msg
viewGameContainer (Internals model) =
    div
        [ css
            [ position relative
            , padding <| px cellMarginSize
            , margin auto
            , marginTop <| px 40
            , backgroundColor <| hex "#BBADA0"
            , borderRadius <| px 6
            , width <| px gameBoardWidth
            , height <| px gameBoardWidth
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
        List.repeat size (viewGridRow size)


viewGridRow : Int -> Html Msg
viewGridRow size =
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , marginBottom <| px cellMarginSize
            ]
        ]
    <|
        List.repeat size (viewGridCell ())


viewGridCell : () -> Html Msg
viewGridCell _ =
    div
        [ css
            [ marginRight <| px cellMarginSize
            , width <| px 106.25
            , height <| px 106.25
            , borderRadius <| px 3
            , backgroundColor <| rgba 238 228 218 0.35
            ]
        ]
        []


viewTiles : Int -> GameGrid -> Html Msg
viewTiles size grid =
    div
        [ css
            [ position absolute
            , zIndex <| int 2
            ]
        ]
        (grid
            |> List.concat
            |> List.filter
                (\value ->
                    case value of
                        Just _ ->
                            True

                        Nothing ->
                            False
                )
            |> List.map (text << Maybe.withDefault "0" << Maybe.map String.fromInt)
         --viewTile
        )



-- viewTile : Maybe Cell -> Html Msg
-- viewTile cell =
--     case cell of
--         Just cellValue ->
--             let
--                 wholeCellSize =
--                     cellSize + cellMarginSize
--                 x =
--                     cellValue.x
--                 y =
--                     cellValue.y
--                 value =
--                     cellValue.value
--             in
--             div
--                 [ css
--                     [ position absolute
--                     , width <| px cellSize
--                     , height <| px cellSize
--                     , lineHeight <| px cellSize
--                     , transform <| translate2 (px (toFloat <| x * wholeCellSize)) (px (toFloat <| y * wholeCellSize))
--                     , Css.Transitions.transition
--                         [ Css.Transitions.transform3 100 0 Css.Transitions.easeInOut ]
--                     ]
--                 ]
--                 [ div
--                     [ css
--                         [ borderRadius <| px 3
--                         , backgroundColor <| hex "EEE4DA"
--                         , textAlign center
--                         , fontWeight bold
--                         , fontSize <| px 55
--                         , zIndex <| int 10
--                         ]
--                     ]
--                     [ text <| String.fromInt value ]
--                 ]
--         Nothing ->
--             text ""


viewFooter : GameGrid -> Html Msg
viewFooter grid =
    let
        cells =
            grid
                |> List.concat
                |> List.map
                    (Maybe.withDefault "0" << Maybe.map String.fromInt)
    in
    div
        []
        [ text <| String.join ", " cells ]



-- SERIALIZATION


keyDecoder : Decoder Msg
keyDecoder =
    D.map toDirection (D.field "key" D.string)


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



-- GETTERS


availableCells : GameGrid -> List ( Int, Int )
availableCells grid =
    let
        withIndexes =
            List.indexedMap Tuple.pair
    in
    grid
        |> withIndexes
        |> List.map (Tuple.mapSecond withIndexes)
        |> List.concatMap (\( rowIndex, row ) -> List.map (\( colIndex, v ) -> ( rowIndex, colIndex, v )) row)
        |> List.filter (\( _, _, v ) -> v == Nothing)
        |> List.map (\( rowIndex, colIndex, _ ) -> ( rowIndex, colIndex ))


isWon : Int -> GameGrid -> Bool
isWon targetScore grid =
    grid
        |> List.concat
        |> List.any (Maybe.withDefault False << Maybe.map ((==) targetScore))


isFailed : Int -> GameGrid -> Bool
isFailed targetScore grid =
    -- not (isWon targetScore grid) && List.
    -- TODO add business logic to calculate if game is failed
    False



-- TRANSFORMERS


withCells : List Cell -> GameGrid -> GameGrid
withCells cells =
    List.indexedMap
        (\rowIndex ->
            List.indexedMap
                (\colIndex val ->
                    case RandomCell.valueAt ( rowIndex, colIndex ) cells of
                        Just newValue ->
                            Just newValue

                        Nothing ->
                            val
                )
        )



-- STYLES


gameBoardWidth : Float
gameBoardWidth =
    500


cellSize : Float
cellSize =
    107


cellMarginSize : Float
cellMarginSize =
    15


occupiedCell : Float
occupiedCell =
    -1000
