module Tile exposing
    ( Tile(..)
    , column
    , merged
    , moveRight
    , randomTiles
    , reverse
    , row
    , sortTilesByColumns
    , sortTilesByRows
    , splitRows
    , transpose
    , value
    , view
    , withKey
    , withMerged
    , withValue
    )

import Css exposing (..)
import Css.Transitions
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Keyed as Keyed
import Random as R
import StyleVariables exposing (..)



-- TYPES


type Tile
    = Tile String Internals


type alias Internals =
    { row : Int
    , column : Int
    , merged : Bool
    , value : Int
    }



-- GETTERS


column : Tile -> Int
column (Tile _ internals) =
    internals.column


row : Tile -> Int
row (Tile _ internals) =
    internals.row


value : Tile -> Int
value (Tile _ internals) =
    internals.value


merged : Tile -> Bool
merged (Tile _ internals) =
    internals.merged



-- TRANSFORM


withKey : List Tile -> Int -> ( List Tile, Int )
withKey tiles initialKey =
    let
        newInitialKey =
            initialKey + List.length tiles

        gotKey index =
            String.fromInt <| initialKey + index

        tileWithKey index (Tile _ internals) =
            Tile (gotKey index) internals
    in
    ( List.indexedMap tileWithKey tiles
    , newInitialKey
    )


withValue : Int -> Tile -> Tile
withValue val (Tile key internals) =
    Tile key { internals | value = val }


withMerged : Bool -> Tile -> Tile
withMerged mrgd (Tile key internals) =
    Tile key { internals | merged = mrgd }


moveRight : Int -> Tile -> Tile
moveRight colIndex (Tile key internals) =
    Tile key { internals | column = colIndex }


sortTilesByRows : List Tile -> List Tile
sortTilesByRows tiles =
    List.sortWith
        (\(Tile _ left) (Tile _ right) ->
            if left.row < right.row || (left.row == right.row && left.column < right.column) then
                LT

            else
                GT
        )
        tiles


sortTilesByColumns : List Tile -> List Tile
sortTilesByColumns tiles =
    List.sortWith
        (\(Tile _ left) (Tile _ right) ->
            if left.column < right.column || (left.column == right.column && left.row < right.row) then
                LT

            else
                GT
        )
        tiles


splitRows : Int -> List Tile -> List (List Tile)
splitRows count tiles =
    case count of
        0 ->
            [ tiles ]

        1 ->
            [ tiles ]

        _ ->
            tiles
                |> sortTilesByRows
                |> getRows count 0
                |> List.filter ((/=) [])


getRows : Int -> Int -> List Tile -> List (List Tile)
getRows count index tiles =
    if index == count then
        []

    else
        let
            ( first, rest ) =
                List.partition (\(Tile _ internals) -> internals.row == index) tiles
        in
        first :: getRows count (index + 1) rest


transpose : List Tile -> List Tile
transpose =
    List.map
        (\(Tile key internals) ->
            Tile key { internals | row = internals.column, column = internals.row }
        )


reverse : Int -> List Tile -> List Tile
reverse maxColumn =
    List.map
        (\(Tile key internals) ->
            Tile key { internals | column = maxColumn - internals.column }
        )



-- GENERATORS


randomValue : R.Generator Int
randomValue =
    R.weighted ( 80, 2 ) [ ( 20, 4 ) ]


randomPosition : List ( Int, Int ) -> R.Generator ( Int, Int )
randomPosition availablePositions =
    case availablePositions of
        [] ->
            R.constant ( 0, 0 )

        first :: rest ->
            R.uniform first rest


randomTile : List ( Int, Int ) -> R.Generator Internals
randomTile availablePositions =
    R.map2
        (\( rowIndex, colIndex ) -> Internals rowIndex colIndex False)
        (randomPosition availablePositions)
        randomValue


randomTiles : Int -> List ( Int, Int ) -> R.Generator (List Tile)
randomTiles count availablePositions =
    randomTile availablePositions
        |> R.list count
        |> R.map (List.map <| Tile "HARDCODED_KEY")



-- VIEW


view : Int -> List Tile -> Html msg
view targetScore tiles =
    Keyed.node "div"
        [ css
            [ position absolute
            , zIndex <| int 2
            ]
        ]
        (List.map (viewKeyedTile targetScore) tiles)


viewKeyedTile : Int -> Tile -> ( String, Html msg )
viewKeyedTile targetScore tile =
    let
        (Tile key _) =
            tile
    in
    ( key, viewTile targetScore tile )


viewTile : Int -> Tile -> Html msg
viewTile targetScore (Tile _ internals) =
    let
        wholeCellSize =
            cellSize + cellMarginSize

        tileBgColor =
            mix <| (logBase 2 (toFloat internals.value) - 1.0) / (logBase 2 (toFloat targetScore) - 1.0) * 100
    in
    div
        [ css
            [ position absolute
            , width <| px cellSize
            , height <| px cellSize
            , lineHeight <| px cellSize
            , transform <|
                translate2
                    (px (toFloat internals.column * wholeCellSize))
                    (px (toFloat internals.row * wholeCellSize))
            , Css.Transitions.transition
                [ Css.Transitions.transform3 100 0 Css.Transitions.easeInOut ]
            ]
        ]
        [ div
            [ css
                [ borderRadius <| px 3
                , backgroundColor <| hex tileBgColor
                , textAlign center
                , fontWeight bold
                , fontSize <| px 55
                , zIndex <| int 10
                ]
            ]
            [ (text << String.fromInt) internals.value ]
        ]
