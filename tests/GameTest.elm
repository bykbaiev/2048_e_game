module GameTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Game exposing (Direction(..), move)
import Test exposing (..)


suite : Test
suite =
    describe "grid recalculation on move"
        [ test "move right (without rotations) 2 * 2"
            (\() ->
                let
                    grid =
                        [ [ Just 2, Nothing ]
                        , [ Just 2, Just 2 ]
                        ]

                    expected =
                        [ [ Nothing, Just 2 ]
                        , [ Nothing, Just 4 ]
                        ]
                in
                Expect.equal (move Right grid) expected
            )
        , test "move right (without rotations) 3 * 3"
            (\() ->
                let
                    grid =
                        [ [ Nothing, Just 2, Nothing ]
                        , [ Nothing, Nothing, Nothing ]
                        , [ Just 2, Just 2, Just 8 ]
                        ]

                    expected =
                        [ [ Nothing, Nothing, Just 2 ]
                        , [ Nothing, Nothing, Nothing ]
                        , [ Nothing, Just 4, Just 8 ]
                        ]
                in
                Expect.equal (move Right grid) expected
            )
        , test "move right (without rotations) 4 * 4"
            (\() ->
                let
                    grid =
                        [ [ Nothing, Nothing, Just 2, Nothing ]
                        , [ Nothing, Nothing, Nothing, Nothing ]
                        , [ Nothing, Nothing, Nothing, Nothing ]
                        , [ Nothing, Just 2, Just 2, Just 8 ]
                        ]

                    expected =
                        [ [ Nothing, Nothing, Nothing, Just 2 ]
                        , [ Nothing, Nothing, Nothing, Nothing ]
                        , [ Nothing, Nothing, Nothing, Nothing ]
                        , [ Nothing, Nothing, Just 4, Just 8 ]
                        ]
                in
                Expect.equal (move Right grid) expected
            )
        , test "move down 4 * 4"
            (\() ->
                let
                    grid =
                        [ [ Nothing, Nothing, Nothing, Nothing ]
                        , [ Just 2, Nothing, Nothing, Nothing ]
                        , [ Just 2, Nothing, Nothing, Just 2 ]
                        , [ Just 8, Nothing, Nothing, Nothing ]
                        ]

                    expected =
                        [ [ Nothing, Nothing, Nothing, Nothing ]
                        , [ Nothing, Nothing, Nothing, Nothing ]
                        , [ Just 4, Nothing, Nothing, Nothing ]
                        , [ Just 8, Nothing, Nothing, Just 2 ]
                        ]
                in
                Expect.equal (move Down grid) expected
            )
        ]
