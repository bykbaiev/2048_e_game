module GameTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game exposing (Direction(..), move)
import Test exposing (..)
import Tile exposing (Tile(..))


suite : Test
suite =
    describe "grid recalculation on move"
        [ test "move right (without rotations) 2 * 2"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 2, row = 0, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 1, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 1, column = 1, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 2, row = 0, column = 1, merged = False }
                        , Tile "key" { value = 4, row = 1, column = 1, merged = False }
                        ]
                in
                Expect.equalLists (move Right 2 tiles) expected
            )
        , test "move right (without rotations) 3 * 3"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 2, row = 0, column = 1, merged = False }
                        , Tile "key" { value = 2, row = 2, column = 0, merged = False }
                        , Tile "key" { value = 2, row = 2, column = 1, merged = False }
                        , Tile "key" { value = 8, row = 2, column = 2, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 2, row = 0, column = 2, merged = False }
                        , Tile "key" { value = 4, row = 2, column = 1, merged = False }
                        , Tile "key" { value = 8, row = 2, column = 2, merged = False }
                        ]
                in
                Expect.equalLists (move Right 3 tiles) expected
            )
        , test "move right (without rotations) 4 * 4"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 2, row = 0, column = 2, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 1, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 2, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 3, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 2, row = 0, column = 3, merged = False }
                        , Tile "key" { value = 4, row = 3, column = 2, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 3, merged = False }
                        ]
                in
                Expect.equalLists (move Right 4 tiles) expected
            )
        , test "move right (without rotations) 4 * 4 (- 4 - 4 | - - 2 8)"
            (\() ->
                let
                    tiles =
                        [ Tile "key" { value = 4, row = 2, column = 1, merged = False }
                        , Tile "key" { value = 4, row = 2, column = 3, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 2, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 3, merged = False }
                        ]

                    expected =
                        [ Tile "key" { value = 8, row = 2, column = 3, merged = False }
                        , Tile "key" { value = 2, row = 3, column = 2, merged = False }
                        , Tile "key" { value = 8, row = 3, column = 3, merged = False }
                        ]
                in
                Expect.equalLists (move Right 4 tiles) expected
            )

        -- , test "move down 4 * 4"
        --     (\() ->
        --         let
        --             grid =
        --                 [ [ Nothing, Nothing, Nothing, Nothing ]
        --                 , [ Just 2, Nothing, Nothing, Nothing ]
        --                 , [ Just 2, Nothing, Nothing, Just 2 ]
        --                 , [ Just 8, Nothing, Nothing, Nothing ]
        --                 ]
        --             expected =
        --                 [ [ Nothing, Nothing, Nothing, Nothing ]
        --                 , [ Nothing, Nothing, Nothing, Nothing ]
        --                 , [ Just 4, Nothing, Nothing, Nothing ]
        --                 , [ Just 8, Nothing, Nothing, Just 2 ]
        --                 ]
        --         in
        --         Expect.equal (move Down grid) expected
        --     )
        ]
