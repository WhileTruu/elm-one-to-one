module OneToOne exposing
    ( OneToOne
    , empty
    , getFirst
    , insert
    , memberFirst
    , removeByFirst
    , removeBySecond
    )

import Dict exposing (Dict)


type OneToOne comparableFirst comparableSecond
    = OneToOne (Dict comparableFirst comparableSecond) (Dict comparableSecond comparableFirst)



-- BUILD


empty : OneToOne comparableFirst comparableSecond
empty =
    OneToOne Dict.empty Dict.empty


insert :
    comparableFirst
    -> comparableSecond
    -> OneToOne comparableFirst comparableSecond
    -> OneToOne comparableFirst comparableSecond
insert first second oneToOne =
    oneToOne
        |> removeByFirst first
        |> removeBySecond second
        |> (\(OneToOne firstDict secondDict) ->
                OneToOne
                    (Dict.insert first second firstDict)
                    (Dict.insert second first secondDict)
           )


removeByFirst :
    comparableFirst
    -> OneToOne comparableFirst comparableSecond
    -> OneToOne comparableFirst comparableSecond
removeByFirst first (OneToOne firstDict secondDict) =
    OneToOne
        (Dict.remove first firstDict)
        (Dict.get first firstDict
            |> Maybe.map (\second -> Dict.remove second secondDict)
            |> Maybe.withDefault secondDict
        )


removeBySecond :
    comparableSecond
    -> OneToOne comparableFirst comparableSecond
    -> OneToOne comparableFirst comparableSecond
removeBySecond second (OneToOne firstDict secondDict) =
    OneToOne
        (Dict.get second secondDict
            |> Maybe.map (\first -> Dict.remove first firstDict)
            |> Maybe.withDefault firstDict
        )
        (Dict.remove second secondDict)



-- QUERY


memberFirst : comparableFirst -> OneToOne comparableFirst comparableSecond -> Bool
memberFirst first (OneToOne firstDict _) =
    Dict.member first firstDict


getFirst :
    comparableSecond
    -> OneToOne comparableFirst comparableSecond
    -> Maybe comparableFirst
getFirst second (OneToOne _ secondDict) =
    Dict.get second secondDict
