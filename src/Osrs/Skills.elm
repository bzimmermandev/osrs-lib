module Osrs.Skills exposing (Skill, Table, emptyTable, experience, experienceForLevel, experienceToNextLevel, level, names, totalExperience, updateTable)

import Array exposing (Array)
import Dict exposing (Dict)


type alias Skill =
    String


type Table
    = Table (Dict Skill Int)


names : List Skill
names =
    [ "Attack"
    , "Strength"
    , "Defense"
    , "Ranged"
    , "Prayer"
    , "Magic"
    , "Runecraft"
    , "Construction"
    , "Hitpoints"
    , "Agility"
    , "Herblore"
    , "Thieving"
    , "Crafting"
    , "Fletching"
    , "Slayer"
    , "Hunter"
    , "Mining"
    , "Smithing"
    , "Fishing"
    , "Cooking"
    , "Firemaking"
    , "Woodcutting"
    , "Farming"
    ]


emptyTable : Table
emptyTable =
    names
        |> List.map (\name -> ( name, 0 ))
        |> Dict.fromList
        |> Table


updateTable : Skill -> Int -> Table -> Table
updateTable skill xp ((Table dict) as table) =
    if List.member skill names then
        -- if given a valid skill name, update the xp entry
        Table (Dict.insert skill xp dict)

    else
        -- otherwise, return the table unaltered
        table


experience : Skill -> Table -> Int
experience skill (Table dict) =
    Dict.get skill dict
        -- if the skill name is invalid, return 0 xp by default
        -- note: this default behavior is to reduce API complexity and should
        --       be sufficient if you're relying on `names` as intended
        |> Maybe.withDefault 0


totalExperience : Table -> Int
totalExperience (Table dict) =
    Dict.values dict
        |> List.sum


experienceToNextLevel : Int -> Int
experienceToNextLevel xp =
    experienceForLevel (level xp + 1)
        -- default to 200m xp as the next level
        |> Maybe.withDefault 200000000
        |> (\nextXp -> nextXp - xp)


experienceForLevel : Int -> Maybe Int
experienceForLevel lvl =
    Array.get (lvl - 1) levelsArray
        |> Maybe.map Tuple.second


level : Int -> Int
level xp =
    Array.foldl
        (\( lvl, milestone ) highestLvl ->
            if xp >= milestone then
                lvl

            else
                highestLvl
        )
        1
        levelsArray



{- helper functions for level calculations -}


levelsArray : Array ( Int, Int )
levelsArray =
    let
        calculateXp index =
            List.range 1 index
                |> List.map (toFloat >> (\x -> x + 300 * 2 ^ (x / 7) |> floor))
                |> List.sum
                |> (\x -> x // 4)
                |> Tuple.pair (index + 1)
    in
    Array.initialize 126 calculateXp
