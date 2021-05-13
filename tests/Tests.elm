module Tests exposing (suite)

import Expect exposing (Expectation)
import Main exposing (seedFromString, seedToString)
import Test exposing (..)


suite : Test
suite =
    describe "seed string conversion"
        [ test "seedFromString"
            (\() ->
                seedFromString "ace"
                    |> Expect.equal (0 * 1 + 2 * 26 + 5 * 26 ^ 2)
            )
        ]
