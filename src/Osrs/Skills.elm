module Osrs.Skills exposing (Experience, Level, Skill, Table, combatLevel, emptyTable, experienceAtLevel, experienceList, get, getLevel, getLevels, level, names, remainingExperience, toDict, totalExperience, totalLevel, update)

import Dict exposing (Dict)
import Dict.Extra



--- TYPES ---


{-| `Skill` is just a convenient alias for `String`, but in the context of this module, it is specifically a `String` containing the (capitalized) name of a skill in the RuneScape Old School skill table. _e.g._, `"Woodcutting"`.

Use `Osrs.Skills.names` for a list of all of the skills currently in the game.

-}
type alias Skill =
    String


{-| `Experience` is an alias for `Int`, and represents experience (XP) for a skill in the game. The alias is used to differentiate an `Int` representing skill XP and an `Int` representing a skill level.
-}
type alias Experience =
    Int


{-| `Level` is an alias for `Int`, and represents a skill level in the game. The alias is used to differentiate an `Int` representing a skill level and an `Int` representing skill XP.
-}
type alias Level =
    Int


type Table
    = Table (Dict Skill Experience)



--- API ---


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


update : Skill -> Experience -> Table -> Table
update skill xp ((Table dict) as table) =
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


get : Skill -> Table -> Experience
get skill (Table dict) =
    Dict.get skill dict
        -- if the skill name is invalid, return 0 xp by default
        -- NOTE this default behavior is to reduce API complexity and should
        --      be sufficient if you're relying on strings from `names` only
        |> Maybe.withDefault 0


getLevel : Skill -> Table -> Level
getLevel skill table =
    get skill table
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



--- HTTP API ---
{-
   {- NOTE These were originally written to fetch and decode a Table from the
     OSRS Hiscores API, but Access-Control-Allow-Origin is not set in the response
     header, meaning that Jagex needs to allow x-origin requests for this API or
     some other hackey solution needs to be written separately from this module
     that allows for these functions to be used. I left the Cmd and the response
     parsing algorithm in case anyone should want to experiment.

     Requires:
       `elm install elm/http`
       `import Http`, `import Result exposing (Result)`
   -}

   fetch : String -> (String -> Result Http.Error Table -> msg) -> Cmd msg
   fetch player messageConstructor =
      let
          endpoint =
              "https://secure.runescape.com/m=hiscore_oldschool/index_lite.ws"
                  ++ "?player="
                  ++ player
      in
      Http.get
          { url = endpoint
          , expect = Http.expectString (Result.map decodeTable >> messageConstructor player)
          }


   decodeTable : String -> Table
   decodeTable string =
      string
          |> String.split " "
          |> List.drop 1
          |> List.take 23
          |> List.map (String.split ",")
          |> List.concatMap (List.drop 2)
          |> List.map String.toInt
          |> List.map (Maybe.withDefault 0)
          |> List.map2 Tuple.pair names
          |> Dict.fromList
          |> Table



-}
--- HELPER FUNCTIONS ---


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
        -- this will be tail-call optimized to avoid accumulating stack frames
        sigmaHelper (x + 1) maxX (acc + f x) f

    else
        acc
