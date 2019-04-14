module Osrs.Skills.Tests exposing (suite)

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Osrs.Skills as Skills
import Test exposing (..)



{- Mockup data -}


mockTable =
    Skills.emptyTable
        |> Skills.updateExperience "Attack" 743664
        |> Skills.updateExperience "Defence" 712396
        |> Skills.updateExperience "Strength" 1560454
        |> Skills.updateExperience "Hitpoints" 1251121
        |> Skills.updateExperience "Ranged" 800212
        |> Skills.updateExperience "Prayer" 334994
        |> Skills.updateExperience "Magic" 653790
        |> Skills.updateExperience "Cooking" 824650
        |> Skills.updateExperience "Woodcutting" 1177033
        |> Skills.updateExperience "Fletching" 782815
        |> Skills.updateExperience "Fishing" 625917
        |> Skills.updateExperience "Firemaking" 834561
        |> Skills.updateExperience "Crafting" 657204
        |> Skills.updateExperience "Smithing" 203301
        |> Skills.updateExperience "Mining" 434723
        |> Skills.updateExperience "Herblore" 201645
        |> Skills.updateExperience "Agility" 751122
        |> Skills.updateExperience "Thieving" 339774
        |> Skills.updateExperience "Slayer" 552844
        |> Skills.updateExperience "Farming" 603234
        |> Skills.updateExperience "Runecraft" 213745
        |> Skills.updateExperience "Hunter" 248541
        |> Skills.updateExperience "Construction" 129927



{- Test suite -}


suite : Test
suite =
    describe "Osrs.Skills"
        [ test "emptyTable |> experienceList returns a properly-sized list of zeros" <|
            \_ ->
                Skills.emptyTable
                    |> Skills.experienceList
                    |> Expect.equal (List.repeat (List.length Skills.names) 0)
        , test "updateExperience |> getExperience returns the correct XP in the updated table" <|
            \_ ->
                Skills.emptyTable
                    |> Skills.updateExperience "Attack" 5000
                    |> Skills.getExperience "Attack"
                    |> Expect.equal 5000
        , test "toDict returns a properly-sized dict" <|
            \_ ->
                Skills.emptyTable
                    |> Skills.toDict
                    |> Dict.size
                    |> Expect.equal (List.length Skills.names)
        , test "experienceAtLevel returns the correct XP for known levels" <|
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
        , test "level returns the correct levels for edge-cases" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Skills.level -1
                            |> Expect.equal 1
                    , \_ ->
                        Skills.level 0
                            |> Expect.equal 1
                    , \_ ->
                        Skills.level 1
                            |> Expect.equal 1
                    , \_ ->
                        Skills.level 200000000
                            |> Expect.equal 126
                    , \_ ->
                        Skills.level 200000001
                            |> Expect.equal 126
                    ]
                    ()
        , test "remainingExperience returns XP-til-200m above virtual level 126" <|
            \_ ->
                Skills.remainingExperience 199999999
                    |> Expect.equal 1
        , describe "Tests using a known table"
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
                                |> Skills.getExperience "Hitpoints"
                                |> Skills.remainingExperience
                                |> Expect.equal 85322
                        , \_ ->
                            mockTable
                                |> Skills.getExperience "Prayer"
                                |> Skills.remainingExperience
                                |> Expect.equal 33605
                        , \_ ->
                            mockTable
                                |> Skills.getExperience "Smithing"
                                |> Skills.remainingExperience
                                |> Expect.equal 21165
                        ]
                        ()
            , test "level returns the correct levels for skills in a known table" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            mockTable
                                |> Skills.getExperience "Hitpoints"
                                |> Skills.level
                                |> Expect.equal 75
                        , \_ ->
                            mockTable
                                |> Skills.getExperience "Hunter"
                                |> Skills.level
                                |> Expect.equal 59
                        ]
                        ()
            , test "getLevel returns the correct levels for skills in a known table" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            mockTable
                                |> Skills.getLevel "Hitpoints"
                                |> Expect.equal 75
                        , \_ ->
                            mockTable
                                |> Skills.getLevel "Hunter"
                                |> Expect.equal 59
                        ]
                        ()
            , test "getLevels returns the correct list of levels for skills in a known table" <|
                \_ ->
                    mockTable
                        |> Skills.getLevels [ "Hitpoints", "Hunter" ]
                        |> Expect.equal [ 75, 59 ]
            , test "totalLevel returns the expected result for a known table" <|
                \_ ->
                    mockTable
                        |> Skills.totalLevel
                        |> Expect.equal 1524
            , test "combatLevel returns the expected result for a known table" <|
                \_ ->
                    mockTable
                        |> Skills.combatLevel
                        |> Expect.within (Absolute 0.0001) 91.525
            ]
        ]
