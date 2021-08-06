module Timer exposing (..)

import Json.Encode as JE
import Swipe


type alias Timer =
    { state : State
    , label : String
    , startTime : Float
    , currentTime : Float
    , tone : AlarmTone
    , vibrate : Bool
    , repetitions : Int
    , currentRepetition : Int
    , restTime : Float
    , currentRestTime : Float
    , color : Color
    , swipe : Swipe.State
    , id : Int
    }


type TimerType
    = Simple
    | Interval


timerTypeToString : TimerType -> String
timerTypeToString type_ =
    case type_ of
        Simple ->
            "Simple"

        Interval ->
            "Interval"


type AlarmTone
    = None
    | NormalAlarm
    | Beep
    | TempleBell
    | Pager
    | Sigh
    | Heels
    | MissleAlarm


alarmTones : List AlarmTone
alarmTones =
    [ None, NormalAlarm, Beep, TempleBell, Pager, Sigh, Heels, MissleAlarm ]


toneToString : AlarmTone -> String
toneToString tone =
    case tone of
        None ->
            "None"

        NormalAlarm ->
            "Normal"

        Beep ->
            "Beep"

        TempleBell ->
            "Temple Bell"

        Pager ->
            "Pager"

        Sigh ->
            "Sigh"

        Heels ->
            "Heels"

        MissleAlarm ->
            "Missle Alarm"


toneToResource : AlarmTone -> String
toneToResource tone =
    case tone of
        None ->
            "none"

        NormalAlarm ->
            "normal.mp3"

        Beep ->
            "beep.mp3"

        TempleBell ->
            "temple_bell.mp3"

        Pager ->
            "pager.mp3"

        Sigh ->
            "sigh.mp3"

        Heels ->
            "heel_walk.mp3"

        MissleAlarm ->
            "missle_alarm.mp3"


type State
    = Counting
    | Paused
    | Ringing
    | RestCounting
    | RestPaused
    | Reset
    | Finished
    | Deleted


timerStateToString : State -> String
timerStateToString state =
    case state of
        Counting ->
            "Counting"

        Paused ->
            "Paused"

        Ringing ->
            "Ringing"

        RestCounting ->
            "RestCounting"

        RestPaused ->
            "RestPaused"

        Reset ->
            "Reset"

        Finished ->
            "Finished"

        Deleted ->
            "Deleted"


encodeTimer : Timer -> JE.Value
encodeTimer timer =
    JE.object
        [ ( "state", JE.string (timerStateToString timer.state) )
        , ( "label", JE.string timer.label )
        , ( "startTime", JE.float timer.startTime )
        , ( "currentTime", JE.float timer.currentTime )
        , ( "tone", JE.string (toneToString timer.tone) )
        , ( "vibrate", JE.bool timer.vibrate )
        , ( "repetitions", JE.int timer.repetitions )
        , ( "currentRepetition", JE.int timer.currentRepetition )
        , ( "restTime", JE.float timer.restTime )
        , ( "currentRestTime", JE.float timer.currentRestTime )
        , ( "id", JE.int timer.id )
        ]


encodeColor : Color -> JE.Value
encodeColor c =
    JE.object
        [ ( "lighter", JE.string c.lighter )
        , ( "darker", JE.string c.darker )
        ]


init : Timer
init =
    { state = Paused
    , label = ""
    , startTime = 0
    , currentTime = 0
    , tone = NormalAlarm
    , vibrate = True
    , repetitions = 1
    , currentRepetition = 1
    , restTime = 0
    , currentRestTime = 0
    , color = salmon
    , swipe = Swipe.init
    , id = -1
    }


type alias Color =
    { lighter : String
    , darker : String
    }


colors : List Color
colors =
    [ salmon
    , purple
    , lightBlue
    , gold
    , orange
    ]


salmon : Color
salmon =
    { lighter = "#D3969A"
    , darker = "#D97179"
    }


purple : Color
purple =
    { lighter = "#756384"
    , darker = "#5E4873"
    }


lightBlue : Color
lightBlue =
    { lighter = "#8CABBF"
    , darker = "#6D90A6"
    }


gold : Color
gold =
    { lighter = "#FDCF89"
    , darker = "#F2B872"
    }


orange : Color
orange =
    { lighter = "#E68866"
    , darker = "#D9663D"
    }
