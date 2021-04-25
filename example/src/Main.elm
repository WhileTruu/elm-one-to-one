module Main exposing (main)

import Browser
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Set exposing (Set)


type alias Model =
    { list : List ( String, Int )
    , activeColor : Maybe String
    , activeNumber : Maybe Int
    }


initialModel : Model
initialModel =
    { list = []
    , activeColor = Nothing
    , activeNumber = Nothing
    }


type Msg
    = NumberClicked Int
    | ColorClicked String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumberClicked number ->
            model.activeColor
                |> Maybe.map
                    (\color ->
                        { model
                            | activeColor = Nothing
                            , activeNumber = Nothing
                            , list = addTo ( color, number ) model.list
                        }
                    )
                |> Maybe.withDefault
                    { model
                        | activeNumber = Just number
                        , list = removeSecond number model.list
                    }

        ColorClicked color ->
            model.activeNumber
                |> Maybe.map
                    (\number ->
                        { model
                            | activeColor = Nothing
                            , activeNumber = Nothing
                            , list = addTo ( color, number ) model.list
                        }
                    )
                |> Maybe.withDefault
                    { model
                        | activeColor = Just color
                        , list = removeFirst color model.list
                    }



-- ONE TO ONE


addTo : ( String, Int ) -> List ( String, Int ) -> List ( String, Int )
addTo pair list =
    list
        |> List.filter
            (\( color, number ) ->
                Tuple.first pair /= color && Tuple.second pair /= number
            )
        |> (::) pair


removeFirst : String -> List ( String, Int ) -> List ( String, Int )
removeFirst first list =
    list
        |> List.filter (Tuple.first >> (/=) first)


removeSecond : Int -> List ( String, Int ) -> List ( String, Int )
removeSecond second list =
    list
        |> List.filter (Tuple.second >> (/=) second)


memberFirst : String -> List ( String, Int ) -> Bool
memberFirst color list =
    List.any (Tuple.first >> (==) color) list


getFirst : Int -> List ( String, Int ) -> Maybe String
getFirst number list =
    List.Extra.find (Tuple.second >> (==) number) list
        |> Maybe.map Tuple.first



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "display" "flex" ]
        [ div []
            (colors
                |> Set.toList
                |> List.map
                    (\color ->
                        div []
                            [ button
                                [ style "padding" "10px"
                                , onClick (ColorClicked color)
                                , if model.activeColor == Just color then
                                    style "border" "5px solid purple"

                                  else
                                    style "" ""
                                , if memberFirst color model.list then
                                    style "background-color" color

                                  else
                                    style "" ""
                                ]
                                [ Html.text color ]
                            ]
                    )
            )
        , div []
            (numbers
                |> Set.toList
                |> List.map
                    (\number ->
                        div []
                            [ button
                                [ style "padding" "10px"
                                , onClick (NumberClicked number)
                                , if model.activeNumber == Just number then
                                    style "border" "5px solid purple"

                                  else
                                    style "" ""
                                , getFirst number model.list
                                    |> Maybe.map (style "background-color")
                                    |> Maybe.withDefault (style "" "")
                                ]
                                [ Html.text (String.fromInt number)
                                ]
                            ]
                    )
            )
        ]


colors : Set String
colors =
    Set.fromList [ "red", "yellow", "blue" ]


numbers : Set Int
numbers =
    Set.fromList [ 1, 2, 3 ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
