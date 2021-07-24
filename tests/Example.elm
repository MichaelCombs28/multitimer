module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Test exposing (..)


suite : Test
suite =
    describe "Utilities"
        [ test "secondsToDigital" <|
            \_ ->
                Expect.equal
                    (Main.secondsToDigital 7379)
                    "02:02:59"
        ]
