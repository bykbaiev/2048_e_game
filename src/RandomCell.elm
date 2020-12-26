module RandomCell exposing (Cell, randomCells, valueAt)

import Random as R



-- TYPES


type Cell
    = Internals Position Value


type alias Position =
    { row : Int
    , column : Int
    }


type alias Value =
    Int



-- GENERATORS


randomValue : R.Generator Value
randomValue =
    R.weighted ( 80, 2 ) [ ( 20, 4 ) ]


randomPosition : List ( Int, Int ) -> R.Generator Position
randomPosition availablePositions =
    case availablePositions of
        [] ->
            R.constant { row = 0, column = 0 }

        first :: rest ->
            R.uniform first rest
                |> R.map (\( row, column ) -> Position row column)


randomCell : List ( Int, Int ) -> R.Generator Cell
randomCell availablePositions =
    R.map2 Internals (randomPosition availablePositions) randomValue


randomCells : Int -> List ( Int, Int ) -> R.Generator (List Cell)
randomCells count availablePositions =
    R.list count <| randomCell availablePositions



-- GETTERS


value : Cell -> Int
value (Internals _ v) =
    v


valueAt : ( Int, Int ) -> List Cell -> Maybe Int
valueAt ( rowIndex, colIndex ) =
    List.head << List.map value << List.filter (\(Internals { row, column } _) -> row == rowIndex && column == colIndex)
