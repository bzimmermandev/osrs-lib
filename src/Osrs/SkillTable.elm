module Osrs.SkillTable exposing (Level, Skill, SkillTable, XP, combatLevel, empty, fetch, getLevel, getLevels, getXp, level, remainingXp, size, skills, toDict, totalLevel, totalXp, update, xpAtLevel)

import Dict exposing (Dict)
import Dict.Extra
import Http



--- TYPES ---


{-| `Skill` is a type alias for `String`, but in the context of this module, it is specifically a `String` containing the (capitalized) name of a skill in the OSRS skill table. _e.g._, `"Woodcutting"`.

Use `skills` when you need a list of all valid skill names.

-}
type alias Skill =
    String


{-| `XP` is a type alias for `Int`, and represents XP (experience) for a skill in the game. The alias is used to differentiate an `Int` representing skill XP and an `Int` representing a skill level in function type annotations.
-}
type alias XP =
    Int


{-| `Level` is a type alias for `Int`, and represents a skill level in OSRS. The alias is used to differentiate an `Int` representing a skill level and an `Int` representing skill XP in function type annotations.
-}
type alias Level =
    Int


{-| `SkillTable` is a datatype representing a complete skill table for one player character. _i.e._, a mapping of each `Skill` in the game to its `XP` and corresponding `Level`. `XP` and `Level` will always stay synchronized.

Nearly all of this module's API is written in terms of a `SkillTable`, so you'll likely want to begin by creating one with `empty`.

-}
type SkillTable
    = SkillTable (Dict Skill XP)



--- API ---


{-| `skills` is a list of all of the valid skill names in the game.

    List.take 4 skills == [ "Attack", "Defence", "Strength", "Hitpoints" ]

-}
skills : List Skill
skills =
    [ "Attack"
    , "Defence"
    , "Strength"
    , "Hitpoints"
    , "Ranged"
    , "Prayer"
    , "Magic"
    , "Cooking"
    , "Woodcutting"
    , "Fletching"
    , "Fishing"
    , "Firemaking"
    , "Crafting"
    , "Smithing"
    , "Mining"
    , "Herblore"
    , "Agility"
    , "Thieving"
    , "Slayer"
    , "Farming"
    , "Runecraft"
    , "Hunter"
    , "Construction"
    ]


{-| An empty `SkillTable`, having 0 XP (level 1) in all skills.
-}
empty : SkillTable
empty =
    skills
        |> List.map (\name -> ( name, 0 ))
        |> Dict.fromList
        |> SkillTable


size : SkillTable -> Int
size (SkillTable dict) =
    Dict.size dict


{-| -}
update : Skill -> XP -> SkillTable -> SkillTable
update skill xp ((SkillTable dict) as table) =
    if List.member skill skills then
        SkillTable (Dict.insert skill xp dict)

    else
        table


toDict : SkillTable -> Dict Skill XP
toDict (SkillTable dict) =
    dict


getXp : Skill -> SkillTable -> XP
getXp skill (SkillTable dict) =
    Dict.get skill dict
        -- if the skill name is invalid, return 0 xp by default
        -- NOTE this default behavior is to reduce API complexity and should
        --      be sufficient if you're relying on strings from `names` only
        |> Maybe.withDefault 0


getLevel : Skill -> SkillTable -> Level
getLevel skill table =
    getXp skill table
        |> level


getLevels : List Skill -> SkillTable -> List Level
getLevels skillList table =
    skillList
        |> List.map (\skill -> getLevel skill table)


level : XP -> Level
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


totalXp : SkillTable -> XP
totalXp (SkillTable dict) =
    Dict.values dict
        |> List.sum


totalLevel : SkillTable -> Level
totalLevel (SkillTable dict) =
    Dict.values dict
        |> List.map level
        |> List.sum


xpAtLevel : Level -> Maybe XP
xpAtLevel lvl =
    Dict.get lvl levelLookup


remainingXp : XP -> XP
remainingXp xp =
    let
        nextLvlXp =
            xpAtLevel (level xp + 1)
                |> Maybe.withDefault 200000000
    in
    nextLvlXp - xp


combatLevel : SkillTable -> Float
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



--- HTTP API ---


fetch : String -> (Result Http.Error SkillTable -> msg) -> Cmd msg
fetch player messageConstructor =
    let
        endpoint =
            "https://crystalmathlabs.com/tracker/api.php?type=datapoints&player="
                ++ String.replace " " "+" player
    in
    Http.get
        { url = endpoint
        , expect = Http.expectString (Result.map decodeTable >> messageConstructor)
        }


decodeTable : String -> SkillTable
decodeTable response =
    -- The response from crystalmathlab's API is awkward to parse
    response
        |> String.lines
        |> List.take 1
        |> List.concatMap (String.split " ")
        |> List.drop 1
        |> List.take 1
        |> List.concatMap (String.split ",")
        |> List.drop 1
        |> List.map (String.toInt >> Maybe.withDefault 0)
        |> List.map2 Tuple.pair skills
        |> Dict.fromList
        |> SkillTable



--- HELPER FUNCTIONS ---


levelLookup : Dict Level XP
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
        -- this will be tail-call optimized to avoid accumulating stack frames
        sigmaHelper (x + 1) maxX (acc + f x) f

    else
        acc
