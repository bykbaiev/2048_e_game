module Game exposing
    ( Direction(..)
    , Model
    , Msg
    , init
    , move
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (onKeyDown)
import Css exposing (..)
import Css.Media
import Css.Transitions
import Html.Attributes exposing (value)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed
import Json.Decode as D exposing (Decoder)
import Random
import StyleVariables exposing (..)
import Tile exposing (Tile, randomTiles)



-- TYPES


type Direction
    = Left
    | Right
    | Up
    | Down


type GameStatus
    = Won
    | Failed
    | InProgress


type Model
    = Internals
        { bestScore : Int
        , size : Int
        , tiles : List Tile
        , score : Int
        , status : GameStatus
        , targetScore : Int
        , nextTileKey : Int
        }



-- MODEL


gridSize : Int
gridSize =
    4


init : () -> ( Model, Cmd Msg )
init () =
    ( Internals
        { bestScore = 0
        , size = gridSize
        , tiles = []
        , score = 0
        , status = InProgress
        , targetScore = 2048
        , nextTileKey = 0
        }
    , Random.generate GotNewTiles (randomTiles 2 (availableTiles gridSize []))
    )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder



-- UPDATE


type Msg
    = KeyDown (Maybe Direction)
    | Reset
    | GotNewTiles (List Tile)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Internals model) =
    case msg of
        KeyDown Nothing ->
            ( Internals model, Cmd.none )

        KeyDown (Just direction) ->
            let
                wonOrFailed =
                    model.status == Won || model.status == Failed

                newTiles =
                    if wonOrFailed then
                        model.tiles

                    else
                        move direction model.size model.tiles

                isNothingChanged =
                    wonOrFailed || model.tiles == newTiles

                -- newGrid =
                -- if won then nothing
                -- if failed then nothing
                -- if nothing changes then nothing
                --
                newModel =
                    if isNothingChanged then
                        Internals model

                    else
                        Internals { model | tiles = newTiles }

                cmd =
                    if isNothingChanged then
                        Cmd.none

                    else
                        Random.generate GotNewTiles (randomTiles 1 (availableTiles model.size newTiles))
            in
            ( newModel, cmd )

        Reset ->
            init ()

        GotNewTiles newTiles ->
            let
                ( keyedTiles, nextTileKey ) =
                    Tile.withKey newTiles model.nextTileKey

                tiles =
                    withTiles keyedTiles model.tiles

                won =
                    isWon model.targetScore tiles

                failed =
                    not won && isFailed model.targetScore model.size tiles
            in
            ( Internals
                { model
                    | tiles = tiles
                    , nextTileKey = nextTileKey
                    , status =
                        if won then
                            Won

                        else if failed then
                            Failed

                        else
                            InProgress
                }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view (Internals model) =
    div
        []
        [ viewHeader model.score model.bestScore
        , viewGameContainer <| Internals model
        , viewFooter
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
        , Tile.view model.tiles
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


viewFooter : Html msg
viewFooter =
    div
        []
        [ text "Awesome game" ]



-- SERIALIZATION


keyDecoder : Decoder Msg
keyDecoder =
    D.map toDirection (D.field "key" D.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            KeyDown (Just Left)

        "ArrowRight" ->
            KeyDown (Just Right)

        "ArrowUp" ->
            KeyDown (Just Up)

        "ArrowDown" ->
            KeyDown (Just Down)

        _ ->
            KeyDown Nothing



-- GETTERS


availableTiles : Int -> List Tile -> List ( Int, Int )
availableTiles size tiles =
    let
        positions : List Int
        positions =
            List.range 0 (size - 1)

        isNotIncluded : ( Int, Int ) -> Bool
        isNotIncluded ( row, column ) =
            tiles
                |> List.filter (\tile -> Tile.column tile == column && Tile.row tile == row)
                |> List.length
                |> (==) 0
    in
    cartesian positions positions
        |> List.filter isNotIncluded


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


isWon : Int -> List Tile -> Bool
isWon targetScore =
    List.any ((==) targetScore << Tile.value)


isFailed : Int -> Int -> List Tile -> Bool
isFailed targetScore size tiles =
    let
        isFull =
            List.length tiles == size

        sortedByRows =
            Tile.sortTilesByRows tiles

        sortedByColumns =
            Tile.sortTilesByColumns tiles

        isMovePossible : List Tile -> Bool
        isMovePossible sortedTiles =
            case sortedTiles of
                [] ->
                    False

                x :: xs ->
                    xs
                        |> List.foldr
                            (\tile { prev, sameValues } ->
                                let
                                    value =
                                        Tile.value tile

                                    row =
                                        Tile.row tile

                                    col =
                                        Tile.column tile

                                    prevValue =
                                        Tile.value prev

                                    prevRow =
                                        Tile.row prev

                                    prevCol =
                                        Tile.column prev
                                in
                                if sameValues then
                                    { prev = prev, sameValues = sameValues }

                                else if prevValue == value && (col == prevCol || row == prevRow) then
                                    { prev = tile, sameValues = True }

                                else
                                    { prev = tile, sameValues = sameValues }
                            )
                            { prev = x, sameValues = False }
                        |> .sameValues
    in
    not (isWon targetScore tiles)
        && isFull
        && (not <| isMovePossible sortedByRows)
        && (not <| isMovePossible sortedByColumns)



-- TRANSFORMERS


withTiles : List Tile -> List Tile -> List Tile
withTiles =
    (++)



-- transpose : GameGrid -> GameGrid
-- transpose grid =
--     case List.head grid of
--         Nothing ->
--             []
--         Just xs ->
--             List.indexedMap
--                 (\index _ ->
--                     List.map
--                         (Maybe.withDefault Nothing
--                             << List.head
--                             << List.filter ((/=) Nothing)
--                             << List.indexedMap
--                                 (\i cell ->
--                                     if i == index then
--                                         cell
--                                     else
--                                         Nothing
--                                 )
--                         )
--                         grid
--                 )
--                 xs
-- rotateClockwise : GameGrid -> GameGrid
-- rotateClockwise =
--     List.map List.reverse << transpose
-- rotateAntiClockwise : GameGrid -> GameGrid
-- rotateAntiClockwise =
--     transpose << List.map List.reverse


move : Direction -> Int -> List Tile -> List Tile
move direction =
    case direction of
        Right ->
            moveRight

        Left ->
            moveRight

        -- rotateClockwise
        --     << rotateClockwise
        --     << moveRight
        --     << rotateClockwise
        --     << rotateClockwise
        Up ->
            moveRight

        -- rotateAntiClockwise << moveRight << rotateClockwise
        Down ->
            moveRight



-- rotateClockwise << moveRight << rotateAntiClockwise


moveRight : Int -> List Tile -> List Tile
moveRight size tiles =
    let
        rows =
            tiles
                |> Tile.splitRows size
                |> List.map (shiftRight <| size - 1)
                |> List.concat

        _ =
            Debug.log "rows" rows
    in
    -- tiles
    rows
        |> Tile.sortTilesByRows
        |> List.foldr
            (\tile accum ->
                let
                    value =
                        Tile.value tile

                    row =
                        Tile.row tile

                    prev : Maybe Tile
                    prev =
                        last accum

                    prevMerged =
                        Maybe.withDefault False <| Maybe.map Tile.merged prev

                    prevValue =
                        Maybe.withDefault 0 <| Maybe.map Tile.value prev

                    prevRow =
                        Maybe.withDefault 0 <| Maybe.map Tile.row prev

                    mergedTile =
                        Tile.withMerged True << Tile.withValue (value * 2)
                in
                if value == prevValue && row == prevRow && not prevMerged then
                    removeLast accum ++ [ mergedTile tile ]

                else
                    accum ++ [ tile ]
            )
            []
        |> List.map (Tile.withMerged False)
        |> Tile.splitRows size
        |> List.map (shiftRight <| size - 1)
        |> List.concat


shiftRight : Int -> List Tile -> List Tile
shiftRight maxColumn =
    List.reverse << List.indexedMap (\index -> Tile.moveRight (maxColumn - index)) << List.reverse


last : List a -> Maybe a
last =
    List.head << List.reverse


removeLast : List a -> List a
removeLast lst =
    let
        length =
            List.length lst
    in
    lst
        |> List.indexedMap Tuple.pair
        |> List.filter (\( index, value ) -> index /= length - 1)
        |> List.map Tuple.second



-- List.map
--     (addEmptyCells
--         << removeEmpties
--         << List.reverse
--         << List.foldr
--             (\cell accum ->
--                 if cell == last accum then
--                     removeLast accum ++ [ Maybe.map ((*) 2) cell, Nothing ]
--                 else
--                     accum ++ [ cell ]
--             )
--             []
--         << addEmptyCells
--         << removeEmpties
--     )
--     grid
-- removeEmpties : Row -> Row
-- removeEmpties =
--     List.filter ((/=) Nothing)
-- addEmpties : Int -> Row -> Row
-- addEmpties size row =
--     List.repeat (size - List.length row) Nothing ++ row
-- removeLast : Row -> Row
-- removeLast row =
--     row
--         |> List.indexedMap Tuple.pair
--         |> List.filter (\( index, value ) -> index /= List.length row - 1)
--         |> List.map Tuple.second
-- last : Row -> Maybe Int
-- last =
--     Maybe.withDefault Nothing << List.head << List.reverse
