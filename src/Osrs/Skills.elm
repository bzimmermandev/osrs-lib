module Osrs.Skills exposing (Experience, Level, Skill, Table, combatLevel, emptyTable, experienceAtLevel, experienceList, getExperience, getLevel, getLevels, level, names, remainingExperience, toDict, totalExperience, totalLevel, updateExperience)

import Dict exposing (Dict)
import Dict.Extra


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


updateExperience : Skill -> Experience -> Table -> Table
updateExperience skill xp ((Table dict) as table) =
    if List.member skill names then
        -- if given a valid skill name, update the xp entry
        Table (Dict.insert skill xp dict)

    else
        -- otherwise, return the table unaltered
        table


toDict : Table -> Dict Skill Experience
toDict (Table dict) =
    dict


experienceList : Table -> List Experience
experienceList (Table dict) =
    Dict.values dict


getExperience : Skill -> Table -> Experience
getExperience skill (Table dict) =
    Dict.get skill dict
        -- if the skill name is invalid, return 0 xp by default
        -- NOTE this default behavior is to reduce API complexity and should
        --      be sufficient if you're relying on strings from `names` only
        |> Maybe.withDefault 0


getLevel : Skill -> Table -> Level
getLevel skill table =
    getExperience skill table
        |> level


getLevels : List Skill -> Table -> List Level
getLevels skills table =
    skills
        |> List.map (\skill -> getLevel skill table)


level : Experience -> Level
level xp =
    let
        clipMinimum minimum x =
            if x < minimum then
                minimum

            else
                x
    in
    Dict.Extra.find (\_ refXp -> refXp > xp) levelLookup
        |> Maybe.map Tuple.first
        |> Maybe.map (\nextLvl -> nextLvl - 1)
        |> Maybe.map (clipMinimum 1)
        |> Maybe.withDefault 126


totalExperience : Table -> Experience
totalExperience table =
    experienceList table
        |> List.sum


totalLevel : Table -> Level
totalLevel table =
    experienceList table
        |> List.map level
        |> List.sum


experienceAtLevel : Level -> Maybe Experience
experienceAtLevel lvl =
    Dict.get lvl levelLookup


remainingExperience : Experience -> Experience
remainingExperience xp =
    let
        nextLvlXp =
            experienceAtLevel (level xp + 1)
                |> Maybe.withDefault 200000000
    in
    nextLvlXp - xp


combatLevel : Table -> Float
combatLevel table =
    let
        prayerLvl =
            getLevel "Prayer" table

        hitpointsLvl =
            getLevel "Hitpoints" table

        defenceLvl =
            getLevel "Defence" table

        strengthLvl =
            getLevel "Strength" table

        attackLvl =
            getLevel "Attack" table

        magicLvl =
            getLevel "Magic" table

        rangedLvl =
            getLevel "Ranged" table

        baseTerm =
            toFloat (prayerLvl // 2 + hitpointsLvl + defenceLvl) / 4

        meleeTerm =
            toFloat (attackLvl + strengthLvl) * 0.325

        rangedTerm =
            toFloat (rangedLvl // 2 + rangedLvl) * 0.325

        magicTerm =
            toFloat (magicLvl // 2 + magicLvl) * 0.325

        maxTerm =
            List.maximum [ meleeTerm, rangedTerm, magicTerm ]
                |> Maybe.withDefault 0
    in
    baseTerm + maxTerm



{- Helper functions -}


levelLookup : Dict Level Experience
levelLookup =
    -- NOTE it may be worthwhile to hardcode the LUT instead of generating it
    let
        calculateXp lvl =
            sigma 1 (lvl - 1) (toFloat >> (\x -> floor (x + 300 * 2 ^ (x / 7))))
                |> (\x -> x // 4)

        insertEntries lvl dict =
            if lvl <= 126 then
                insertEntries (lvl + 1) dict
                    |> Dict.insert lvl (calculateXp lvl)

            else
                dict
    in
    insertEntries 1 Dict.empty


sigma : Int -> Int -> (Int -> number) -> number
sigma x maxX f =
    sigmaHelper x maxX 0 f


sigmaHelper : Int -> Int -> number -> (Int -> number) -> number
sigmaHelper x maxX acc f =
    if x <= maxX then
        -- this call will be tail-call optimized to avoid accumulating
        -- stack frames
        sigmaHelper (x + 1) maxX (acc + f x) f

    else
        acc
