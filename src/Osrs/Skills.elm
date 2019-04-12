module Osrs.Skills exposing (Experience, Level, Skill, Table, emptyTable, experience, experienceAtLevel, experienceList, level, names, remainingExperience, totalExperience, updateTable)

import Array exposing (Array)
import Dict exposing (Dict)


type alias Skill =
    String


type alias Experience =
    Int


type alias Level =
    Int


type Table
    = Table (Dict Skill Experience)


names : List Skill
names =
    [ "Attack"
    , "Strength"
    , "Defence"
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


updateTable : Skill -> Experience -> Table -> Table
updateTable skill xp ((Table dict) as table) =
    if List.member skill names then
        -- if given a valid skill name, update the xp entry
        Table (Dict.insert skill xp dict)

    else
        -- otherwise, return the table unaltered
        table


experience : Skill -> Table -> Experience
experience skill (Table dict) =
    Dict.get skill dict
        -- if the skill name is invalid, return 0 xp by default
        -- note: this default behavior is to reduce API complexity and should
        --       be sufficient if you're relying on strings from `names` only
        |> Maybe.withDefault 0


experienceList : Table -> List Experience
experienceList (Table dict) =
    Dict.values dict


totalExperience : Table -> Experience
totalExperience table =
    experienceList table
        |> List.sum


remainingExperience : Experience -> Experience
remainingExperience xp =
    experienceAtLevel (level xp + 1)
        -- default to 200m xp as the next level
        |> Maybe.withDefault 200000000
        |> (\nextXp -> nextXp - xp)


experienceAtLevel : Level -> Maybe Experience
experienceAtLevel lvl =
    Array.get (lvl - 1) levelLookup
        |> Maybe.map Tuple.second


level : Experience -> Level
level xp =
    Array.foldl
        (\( lvl, milestone ) highestLvl ->
            if xp >= milestone then
                lvl

            else
                highestLvl
        )
        1
        levelLookup



{- helper functions for level calculations -}


levelLookup : Array ( Level, Experience )
levelLookup =
    -- NOTE it may make more sense to hardcode the array instead of generating it
    let
        calculateXp index =
            List.range 1 index
                |> List.map (toFloat >> (\x -> x + 300 * 2 ^ (x / 7) |> floor))
                |> List.sum
                |> (\x -> x // 4)
                |> Tuple.pair (index + 1)
    in
    Array.initialize 126 calculateXp
