module Tests exposing (..)

import Expect
import Fuzz
import OneToOne exposing (OneToOne)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "OneToOne"
        [ Test.fuzz (Fuzz.list (Fuzz.tuple ( Fuzz.string, Fuzz.int )))
            "converting to list and back results in same mapping"
            (\list ->
                let
                    stringToInt : OneToOne String Int
                    stringToInt =
                        OneToOne.fromList list
                in
                Expect.equal
                    stringToInt
                    (OneToOne.fromList (OneToOne.toList stringToInt))
            )
        ]
