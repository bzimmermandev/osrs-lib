module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Osrs.Skills as Skills
import Test exposing (..)


suite : Test
suite =
    describe "Osrs.Skills"
        [ test "emptyTable |> experienceList returns a properly-sized list of zeros" <|
            \_ ->
                Skills.emptyTable
                    |> Skills.experienceList
                    |> Expect.equal (List.repeat (List.length Skills.names) 0)
        , test "updateTable |> experience returns the correct xp in the updated table" <|
            \_ ->
                Skills.emptyTable
                    |> Skills.updateTable "Attack" 5000
                    |> Skills.experience "Attack"
                    |> Expect.equal 5000
        , test "experienceAtLevel returns the correct xp for known levels" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Skills.experienceAtLevel 1
                            |> Expect.equal (Just 0)
                    , \_ ->
                        Skills.experienceAtLevel 80
                            |> Expect.equal (Just 1986068)
                    , \_ ->
                        Skills.experienceAtLevel 90
                            |> Expect.equal (Just 5346332)
                    , \_ ->
                        Skills.experienceAtLevel 126
                            |> Expect.equal (Just 188884740)
                    ]
                    ()
        , test "experienceAtLevel returns Nothing for levels that are out-of-bounds (1 <= level <= 126)" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Skills.experienceAtLevel -1
                            |> Expect.equal Nothing
                    , \_ ->
                        Skills.experienceAtLevel 127
                            |> Expect.equal Nothing
                    ]
                    ()
        , let
            mockTable =
                Skills.emptyTable
                    |> Skills.updateTable "Attack" 743664
                    |> Skills.updateTable "Defence" 712396
                    |> Skills.updateTable "Strength" 1560454
                    |> Skills.updateTable "Hitpoints" 1251121
                    |> Skills.updateTable "Ranged" 800212
                    |> Skills.updateTable "Prayer" 334994
                    |> Skills.updateTable "Magic" 653790
                    |> Skills.updateTable "Cooking" 824650
                    |> Skills.updateTable "Woodcutting" 1177033
                    |> Skills.updateTable "Fletching" 782815
                    |> Skills.updateTable "Fishing" 625917
                    |> Skills.updateTable "Firemaking" 834561
                    |> Skills.updateTable "Crafting" 657204
                    |> Skills.updateTable "Smithing" 203301
                    |> Skills.updateTable "Mining" 434723
                    |> Skills.updateTable "Herblore" 201645
                    |> Skills.updateTable "Agility" 751122
                    |> Skills.updateTable "Thieving" 339774
                    |> Skills.updateTable "Slayer" 552844
                    |> Skills.updateTable "Farming" 603234
                    |> Skills.updateTable "Runecraft" 213745
                    |> Skills.updateTable "Hunter" 248541
                    |> Skills.updateTable "Construction" 129927
          in
          describe "Tests using a known `Table`"
            [ test "totalExperience returns the expected result for a known table" <|
                \_ ->
                    mockTable
                        |> Skills.totalExperience
                        |> Expect.equal 14637667
            , test "remainingExperience calculates correctly for a skill in a known table" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            mockTable
                                |> Skills.experience "Hitpoints"
                                |> Skills.remainingExperience
                                |> Expect.equal 85322
                        , \_ ->
                            mockTable
                                |> Skills.experience "Prayer"
                                |> Skills.remainingExperience
                                |> Expect.equal 33605
                        , \_ ->
                            mockTable
                                |> Skills.experience "Smithing"
                                |> Skills.remainingExperience
                                |> Expect.equal 21165
                        ]
                        ()
            , test "remainingExperience returns the xp til 200m above virtual level 126" <|
                \_ ->
                    Skills.remainingExperience 199999999
                        |> Expect.equal 1
            , test "level returns the correct skill levels for skills in a known table" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            mockTable
                                |> Skills.experience "Hitpoints"
                                |> Skills.level
                                |> Expect.equal 75
                        , \_ ->
                            mockTable
                                |> Skills.experience "Hunter"
                                |> Skills.level
                                |> Expect.equal 59
                        ]
                        ()
            ]
        ]
