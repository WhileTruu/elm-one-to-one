module Main exposing (main)

import Browser
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import OneToOne exposing (OneToOne)
import Set exposing (Set)



-- MODEL


type alias Model =
    { colorToNumber : OneToOne String Int
    , activeColor : Maybe String
    , activeNumber : Maybe Int
    }


initialModel : Model
initialModel =
    { colorToNumber = OneToOne.empty
    , activeColor = Nothing
    , activeNumber = Nothing
    }



-- UPDATE


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
                            , colorToNumber = OneToOne.insert color number model.colorToNumber
                        }
                    )
                |> Maybe.withDefault
                    { model
                        | activeNumber = Just number
                        , colorToNumber = OneToOne.removeSecond number model.colorToNumber
                    }

        ColorClicked color ->
            model.activeNumber
                |> Maybe.map
                    (\number ->
                        { model
                            | activeColor = Nothing
                            , activeNumber = Nothing
                            , colorToNumber = OneToOne.insert color number model.colorToNumber
                        }
                    )
                |> Maybe.withDefault
                    { model
                        | activeColor = Just color
                        , colorToNumber = OneToOne.removeFirst color model.colorToNumber
                    }



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
                                , if OneToOne.memberFirst color model.colorToNumber then
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
                                , OneToOne.first number model.colorToNumber
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



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
