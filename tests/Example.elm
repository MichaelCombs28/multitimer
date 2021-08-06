module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main
import Swipe
import Test exposing (..)
import Timer


suite : Test
suite =
    describe "Utilities"
        [ test "secondsToDigital" <|
            \_ ->
                Expect.equal
                    (Main.millisToDigital 7379000)
                    "02:02:59"
        , test "fromActive perfect" <|
            \_ ->
                Expect.equal
                    { state = Timer.Reset
                    , label = "test"
                    , startTime = 10000
                    , currentTime = 0
                    , tone = Timer.None
                    , vibrate = True
                    , repetitions = 3
                    , currentRepetition = 3
                    , restTime = 0
                    , currentRestTime = 0
                    , color = Timer.salmon
                    , swipe = Swipe.init
                    , id = 0
                    }
                    (Main.fromActive 30000
                        { state = Timer.Counting
                        , label = "test"
                        , startTime = 10000
                        , currentTime = 10000
                        , tone = Timer.None
                        , vibrate = True
                        , repetitions = 3
                        , currentRepetition = 1
                        , restTime = 0
                        , currentRestTime = 0
                        , color = Timer.salmon
                        , swipe = Swipe.init
                        , id = 0
                        }
                    )
        , test "fromActive one rep left" <|
            \_ ->
                Expect.equal
                    { state = Timer.Counting
                    , label = "test"
                    , startTime = 10000
                    , currentTime = 10000
                    , tone = Timer.None
                    , vibrate = True
                    , repetitions = 3
                    , currentRepetition = 3
                    , restTime = 0
                    , currentRestTime = 0
                    , color = Timer.salmon
                    , swipe = Swipe.init
                    , id = 0
                    }
                    (Main.fromActive 20000
                        { state = Timer.Counting
                        , label = "test"
                        , startTime = 10000
                        , currentTime = 10000
                        , tone = Timer.None
                        , vibrate = True
                        , repetitions = 3
                        , currentRepetition = 1
                        , restTime = 0
                        , currentRestTime = 0
                        , color = Timer.salmon
                        , swipe = Swipe.init
                        , id = 0
                        }
                    )
        , test "fromActive two reps left" <|
            \_ ->
                Expect.equal
                    { state = Timer.Counting
                    , label = "test"
                    , startTime = 10000
                    , currentTime = 10000
                    , tone = Timer.None
                    , vibrate = True
                    , repetitions = 4
                    , currentRepetition = 3
                    , restTime = 0
                    , currentRestTime = 0
                    , color = Timer.salmon
                    , swipe = Swipe.init
                    , id = 0
                    }
                    (Main.fromActive 20000
                        { state = Timer.Counting
                        , label = "test"
                        , startTime = 10000
                        , currentTime = 10000
                        , tone = Timer.None
                        , vibrate = True
                        , repetitions = 4
                        , currentRepetition = 1
                        , restTime = 0
                        , currentRestTime = 0
                        , color = Timer.salmon
                        , swipe = Swipe.init
                        , id = 0
                        }
                    )
        , test "fromActive with rest time" <|
            \_ ->
                Expect.equal
                    { state = Timer.Counting
                    , label = "test"
                    , startTime = 10000
                    , currentTime = 10000
                    , tone = Timer.None
                    , vibrate = True
                    , repetitions = 4
                    , currentRepetition = 2
                    , restTime = 10000
                    , currentRestTime = 10000
                    , color = Timer.salmon
                    , swipe = Swipe.init
                    , id = 0
                    }
                    (Main.fromActive 20000
                        { state = Timer.Counting
                        , label = "test"
                        , startTime = 10000
                        , currentTime = 10000
                        , tone = Timer.None
                        , vibrate = True
                        , repetitions = 4
                        , currentRepetition = 1
                        , restTime = 10000
                        , currentRestTime = 0
                        , color = Timer.salmon
                        , swipe = Swipe.init
                        , id = 0
                        }
                    )
        , test "fromActive still resting" <|
            \_ ->
                Expect.equal
                    { state = Timer.RestCounting
                    , label = "test"
                    , startTime = 20000
                    , currentTime = 20000
                    , tone = Timer.None
                    , vibrate = True
                    , repetitions = 4
                    , currentRepetition = 2
                    , restTime = 20000
                    , currentRestTime = 10000
                    , color = Timer.salmon
                    , swipe = Swipe.init
                    , id = 0
                    }
                    (Main.fromActive 30000
                        { state = Timer.Counting
                        , label = "test"
                        , startTime = 20000
                        , currentTime = 20000
                        , tone = Timer.None
                        , vibrate = True
                        , repetitions = 4
                        , currentRepetition = 2
                        , restTime = 20000
                        , currentRestTime = 20000
                        , color = Timer.salmon
                        , swipe = Swipe.init
                        , id = 0
                        }
                    )
        ]
