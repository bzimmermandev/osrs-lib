module Osrs.SkillTable.Tests exposing (suite)

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Osrs.SkillTable as SkillTable
import Test exposing (..)



{- Mockup data -}


mockTable =
    SkillTable.empty
        |> SkillTable.update "Attack" 743664
        |> SkillTable.update "Defence" 712396
        |> SkillTable.update "Strength" 1560454
        |> SkillTable.update "Hitpoints" 1251121
        |> SkillTable.update "Ranged" 800212
        |> SkillTable.update "Prayer" 334994
        |> SkillTable.update "Magic" 653790
        |> SkillTable.update "Cooking" 824650
        |> SkillTable.update "Woodcutting" 1177033
        |> SkillTable.update "Fletching" 782815
        |> SkillTable.update "Fishing" 625917
        |> SkillTable.update "Firemaking" 834561
        |> SkillTable.update "Crafting" 657204
        |> SkillTable.update "Smithing" 203301
        |> SkillTable.update "Mining" 434723
        |> SkillTable.update "Herblore" 201645
        |> SkillTable.update "Agility" 751122
        |> SkillTable.update "Thieving" 339774
        |> SkillTable.update "Slayer" 552844
        |> SkillTable.update "Farming" 603234
        |> SkillTable.update "Runecraft" 213745
        |> SkillTable.update "Hunter" 248541
        |> SkillTable.update "Construction" 129927



{- Test suite -}


suite : Test
suite =
    describe "Osrs.SkillTable"
        [ test "empty |> size returns a properly-sized table" <|
            \_ ->
                SkillTable.empty
                    |> SkillTable.size
                    |> Expect.equal (List.length SkillTable.skills)
        , test "update |> getXp returns the correct XP in the updated table" <|
            \_ ->
                SkillTable.empty
                    |> SkillTable.update "Attack" 5000
                    |> SkillTable.getXp "Attack"
                    |> Expect.equal 5000
        , test "xpAtLevel returns the correct XP for known levels" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        SkillTable.xpAtLevel 1
                            |> Expect.equal (Just 0)
                    , \_ ->
                        SkillTable.xpAtLevel 80
                            |> Expect.equal (Just 1986068)
                    , \_ ->
                        SkillTable.xpAtLevel 90
                            |> Expect.equal (Just 5346332)
                    , \_ ->
                        SkillTable.xpAtLevel 126
                            |> Expect.equal (Just 188884740)
                    ]
                    ()
        , test "experienceAtLevel returns Nothing for levels that are out-of-bounds (1 <= level <= 126)" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        SkillTable.xpAtLevel -1
                            |> Expect.equal Nothing
                    , \_ ->
                        SkillTable.xpAtLevel 127
                            |> Expect.equal Nothing
                    ]
                    ()
        , test "level returns the correct levels for edge-cases" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        SkillTable.level -1
                            |> Expect.equal 1
                    , \_ ->
                        SkillTable.level 0
                            |> Expect.equal 1
                    , \_ ->
                        SkillTable.level 1
                            |> Expect.equal 1
                    , \_ ->
                        SkillTable.level 200000000
                            |> Expect.equal 126
                    , \_ ->
                        SkillTable.level 200000001
                            |> Expect.equal 126
                    ]
                    ()
        , test "remainingExperience returns XP-til-200m above virtual level 126" <|
            \_ ->
                SkillTable.remainingXp 199999999
                    |> Expect.equal 1
        , describe "Tests using a known SkillTable"
            [ test "totalExperience returns the expected result for a known table" <|
                \_ ->
                    mockTable
                        |> SkillTable.totalXp
                        |> Expect.equal 14637667
            , test "remainingExperience calculates correctly for a skill in a known table" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            mockTable
                                |> SkillTable.getXp "Hitpoints"
                                |> SkillTable.remainingXp
                                |> Expect.equal 85322
                        , \_ ->
                            mockTable
                                |> SkillTable.getXp "Prayer"
                                |> SkillTable.remainingXp
                                |> Expect.equal 33605
                        , \_ ->
                            mockTable
                                |> SkillTable.getXp "Smithing"
                                |> SkillTable.remainingXp
                                |> Expect.equal 21165
                        ]
                        ()
            , test "level returns the correct levels for skills in a known table" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            mockTable
                                |> SkillTable.getXp "Hitpoints"
                                |> SkillTable.level
                                |> Expect.equal 75
                        , \_ ->
                            mockTable
                                |> SkillTable.getXp "Hunter"
                                |> SkillTable.level
                                |> Expect.equal 59
                        ]
                        ()
            , test "getLevel returns the correct levels for skills in a known table" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            mockTable
                                |> SkillTable.getLevel "Hitpoints"
                                |> Expect.equal 75
                        , \_ ->
                            mockTable
                                |> SkillTable.getLevel "Hunter"
                                |> Expect.equal 59
                        ]
                        ()
            , test "getLevels returns the correct list of levels for skills in a known table" <|
                \_ ->
                    mockTable
                        |> SkillTable.getLevels [ "Hitpoints", "Hunter" ]
                        |> Expect.equal [ 75, 59 ]
            , test "totalLevel returns the expected result for a known table" <|
                \_ ->
                    mockTable
                        |> SkillTable.totalLevel
                        |> Expect.equal 1524
            , test "combatLevel returns the expected result for a known table" <|
                \_ ->
                    mockTable
                        |> SkillTable.combatLevel
                        |> Expect.within (Absolute 0.0001) 91.525
            ]
        ]
