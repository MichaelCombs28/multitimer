port module Ports exposing (..)

import Json.Decode as JD
import Json.Encode as JE
import Timer exposing (Timer)



-- OUTGOING


port fromElm : ElmEvent -> Cmd msg


type alias ElmEvent =
    JE.Value


keepAwake : ElmEvent
keepAwake =
    withType "keepAwake" []


allowSleep : ElmEvent
allowSleep =
    withType "allowSleep" []


alarmStarted : Timer -> ElmEvent
alarmStarted timer =
    withType "alarmStarted"
        [ ( "id", JE.int timer.id )
        , ( "state", JE.string <| Timer.timerStateToString timer.state )
        , ( "label", JE.string timer.label )
        , ( "vibrate", JE.bool timer.vibrate )
        , ( "startTime", JE.float timer.startTime )
        , ( "currentTime", JE.float timer.currentTime )
        , ( "restTime", JE.float timer.restTime )
        , ( "currentRestTime", JE.float timer.currentRestTime )
        , ( "repetitions", JE.int timer.repetitions )
        , ( "currentRepetition", JE.int timer.currentRepetition )
        , ( "tone", JE.string <| Timer.toneToResource timer.tone )
        ]


alarmStopped : Timer -> ElmEvent
alarmStopped timer =
    withType "alarmStopped"
        [ ( "id", JE.int timer.id ) ]


ring : Timer -> ElmEvent
ring timer =
    withType "ring"
        [ ( "id", JE.int timer.id )
        , ( "label", JE.string timer.label )
        , ( "tone", JE.string <| Timer.toneToResource timer.tone )
        , ( "vibrate", JE.bool timer.vibrate )
        , ( "duration", JE.float 1000 )
        , ( "interval", JE.float 2000 )
        ]


stopRing : Timer -> ElmEvent
stopRing timer =
    withType "stopRing"
        [ ( "id", JE.int timer.id ) ]


hapticVibrate : Float -> ElmEvent
hapticVibrate duration =
    withType "hapticVibrate"
        [ ( "duration", JE.float duration ) ]


timers : List Timer -> ElmEvent
timers timers_ =
    withType "timers"
        [ ( "timers", JE.list Timer.encodeTimer timers_ ) ]


nowActive : ElmEvent
nowActive =
    withType "nowActive" []


storage : String -> JE.Value -> ElmEvent
storage storageKey value =
    withType "storage"
        [ ( "key", JE.string storageKey )
        , ( "value", value )
        ]


withType : String -> List ( String, JE.Value ) -> ElmEvent
withType typeName values =
    ( "type", JE.string typeName )
        :: values
        |> JE.object



-- INCOMING


port fromJS : (JE.Value -> msg) -> Sub msg


type JSEvent
    = NotificationClosed { id : Int }
    | AppStateChanged { isActive : Bool }
    | BackgroundTimerUpdates { timers : List BackgroundTimerUpdate }


type alias BackgroundTimerUpdate =
    { id : Int
    , state : Timer.State
    , currentTime : Float
    , currentRestTime : Float
    , currentRepetition : Int
    }


backgroundTimerUpdateDecoder : JD.Decoder BackgroundTimerUpdate
backgroundTimerUpdateDecoder =
    JD.map5 BackgroundTimerUpdate
        (JD.field "id" JD.int)
        (JD.field "state" timerStateDecoder)
        (JD.field "currentTime" JD.float)
        (JD.field "currentRestTime" JD.float)
        (JD.field "currentRepetition" JD.int)


timerStateDecoder : JD.Decoder Timer.State
timerStateDecoder =
    JD.string
        |> JD.andThen
            (\state ->
                case state of
                    "Counting" ->
                        JD.succeed Timer.Counting

                    "Paused" ->
                        JD.succeed Timer.Paused

                    "Ringing" ->
                        JD.succeed Timer.Ringing

                    "RestCounting" ->
                        JD.succeed Timer.RestCounting

                    "RestPaused" ->
                        JD.succeed Timer.RestPaused

                    "Reset" ->
                        JD.succeed Timer.Reset

                    "Finished" ->
                        JD.succeed Timer.Finished

                    "Deleted" ->
                        JD.succeed Timer.Deleted

                    _ ->
                        JD.fail (state ++ " is not a valid timer state.")
            )


jsEventDecoder : JD.Decoder JSEvent
jsEventDecoder =
    JD.field "type" JD.string
        |> JD.andThen jsEventDecoder_


jsEventDecoder_ : String -> JD.Decoder JSEvent
jsEventDecoder_ type_ =
    case type_ of
        "NotificationClosed" ->
            JD.map NotificationClosed alarmStoppedDecoder

        "AppStateChanged" ->
            JD.map AppStateChanged appStateChangedDecoder

        "BackgroundTimerUpdates" ->
            JD.map BackgroundTimerUpdates backgroundTimerUpdates

        _ ->
            JD.fail (type_ ++ " is not handled.")


alarmStoppedDecoder : JD.Decoder { id : Int }
alarmStoppedDecoder =
    JD.field "id" JD.int
        |> JD.map (\i -> { id = i })


appStateChangedDecoder : JD.Decoder { isActive : Bool }
appStateChangedDecoder =
    JD.field "isActive" JD.bool
        |> JD.map (\i -> { isActive = i })


backgroundTimerUpdates : JD.Decoder { timers : List BackgroundTimerUpdate }
backgroundTimerUpdates =
    JD.field "timers" (JD.list backgroundTimerUpdateDecoder)
        |> JD.map (\i -> { timers = i })
