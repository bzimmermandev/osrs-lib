module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Osrs.Skills as Skills
import Test exposing (..)


suite : Test
suite =
    describe "Osrs.Skills"
        [ test "emptyTable returns an empty table" <|
            \_ ->
                Skills.emptyTable
                    |> Skills.experienceList
                    |> Expect.equal (List.repeat (List.length Skills.names) 0)
        ]
