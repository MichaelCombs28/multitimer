module Main exposing (..)

import Browser
import Browser.Events
import Css
import Css.Animations as Animations
import Css.Global as CG
import Css.Media exposing (screen)
import Css.Transitions as Transitions
import Html.Events as ElementEvents
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (class, css)
import Html.Styled.Events as Events
import Icon
import Json.Decode as JD
import Json.Encode as JE
import Ports
import Process
import Swipe
import Task exposing (Task)
import Time
import Timer exposing (Timer)


main : Program (Maybe { lastScreen : String }) App Msg
main =
    Browser.element
        { init = init >> Tuple.mapFirst Active
        , update = updateApp
        , view = appView >> Html.toUnstyled
        , subscriptions = subscriptions
        }



-- App


type App
    = Active Model
    | Inactive Time.Posix Model


updateApp : Msg -> App -> ( App, Cmd Msg )
updateApp msg app =
    case app of
        Active model ->
            case msg of
                AppStateChangedInactive time ->
                    ( Inactive time model, Cmd.none )

                _ ->
                    update msg model
                        |> Tuple.mapFirst Active

        Inactive lastTime model ->
            case msg of
                JSEventReceived (Ports.AppStateChanged _) ->
                    -- TODO Subtract time from clocks
                    ( app, Time.now |> Task.perform AppStateChangedActive )

                AppStateChangedActive currentTime ->
                    let
                        timers =
                            List.map (fromActive (toFloat <| Time.posixToMillis currentTime - Time.posixToMillis lastTime))
                                model.timers
                    in
                    ( Active { model | timers = timers }, Cmd.none )

                _ ->
                    ( app, Cmd.none )


appView : App -> Html Msg
appView app =
    case app of
        Active model ->
            view model

        Inactive _ _ ->
            Html.text ""



-- MODEL


type alias Model =
    { screen : Screen
    , loaded : Bool
    , lastScreen : Screen
    , timers : List Timer
    , nextId : Int
    , tempTimer : Timer
    , startTimeWheels : List WheelSelector
    , restTimeWheels : List PickerState
    , repetitionModalOpen : Bool
    , repetitionPickerPosition : PickerState
    , toneModalOpen : Bool
    , tonePickerPosition : PickerState
    , restTimeModalOpen : Bool
    }


init : Maybe { lastScreen : String } -> ( Model, Cmd Msg )
init ls =
    let
        ( screen, cmds_ ) =
            case ls of
                Just { lastScreen } ->
                    case screenFromString lastScreen of
                        Just s ->
                            ( s, Cmd.none )

                        Nothing ->
                            ( Splash, Process.sleep 100 |> Task.perform (always Load) )

                Nothing ->
                    ( Splash, Process.sleep 100 |> Task.perform (always Load) )

        cmds =
            Cmd.batch
                [ Ports.fromElm Ports.keepAwake, cmds_ ]
    in
    ( { screen = screen
      , loaded = False
      , lastScreen = Thumbnail Normal
      , timers = []
      , nextId = 0
      , tempTimer = Timer.init
      , startTimeWheels = initWheel
      , restTimeWheels = List.repeat 3 initPickerState
      , repetitionModalOpen = False
      , repetitionPickerPosition = initPickerState
      , toneModalOpen = False
      , tonePickerPosition =
            { initPickerState
                | index = 1
                , currentPosition = -32
            }
      , restTimeModalOpen = False
      }
    , cmds
    )


initWheel : List WheelSelector
initWheel =
    List.map initWheelSelector [ 99, 59, 59 ]


type Screen
    = Splash
    | Credits
    | Thumbnail Mode
    | Countdown Mode
    | TimerAdd Timer.TimerType
    | TimerEdit Int Timer.TimerType


screenToString : Screen -> String
screenToString screen =
    case screen of
        Splash ->
            "Splash"

        Credits ->
            "Credits"

        Thumbnail _ ->
            "Thumbnail"

        Countdown _ ->
            "Countdown"

        TimerAdd _ ->
            "TimerAdd"

        TimerEdit _ _ ->
            "TimerEdit"


screenFromString : String -> Maybe Screen
screenFromString screen =
    case screen of
        "Splash" ->
            Just Splash

        "Credits" ->
            Just Credits

        "Thumbnail" ->
            Thumbnail Normal
                |> Just

        "Countdown" ->
            Countdown Normal
                |> Just

        _ ->
            Nothing


type Mode
    = Normal
    | Editing



-- SUBSCRIPTIONS
-- TODO, try CSS without on animation frame, likely better on the browser and easier to implement tbh


anyCounting : List Timer -> Bool
anyCounting timers =
    List.any (.state >> (\a -> a == Timer.Counting || a == Timer.RestCounting)) timers


subscriptions : App -> Sub Msg
subscriptions app =
    case app of
        Active model ->
            let
                timer =
                    if anyCounting model.timers then
                        Browser.Events.onAnimationFrameDelta Tick

                    else
                        Sub.none

                animationFrame =
                    if List.any .decelerating model.startTimeWheels then
                        Browser.Events.onAnimationFrameDelta MomentumStep

                    else
                        Sub.none
            in
            Sub.batch
                [ timer
                , animationFrame
                , Ports.fromJS jsEventPortTransformer
                ]

        Inactive _ _ ->
            Ports.fromJS jsEventPortTransformer


jsEventPortTransformer : JE.Value -> Msg
jsEventPortTransformer value =
    case JD.decodeValue Ports.jsEventDecoder value of
        Ok evt ->
            JSEventReceived evt

        Err e ->
            Debug.todo (Debug.toString e) ()



-- UPDATE


type Msg
    = NoOp
    | Load
    | Start
    | SwipeMsg Int Swipe.State
    | DeleteTimer Int
    | ScreenSelected Bool Screen Screen
    | Tick Float
    | AlarmStopped Int
    | AlarmReset Int
    | ResetAll
    | PauseAll
    | PlayAll
    | ToggleState Int Timer.State
    | MomentumStart Int Float
    | DragStep Int Float
    | MomentumStop Int
    | WheelUpdated Int WheelSelector
    | MomentumStep Float
    | RestWheelUpdated Int PickerState
    | ColorSelected Timer.Color
    | InputName String
    | InputVibrate Bool
    | ToneSelected Timer.AlarmTone
    | Done
    | RepetitionModalOpened Bool
    | ToneModalOpened Bool
    | RestTimeModalOpened Bool
    | RepetitionPositionUpdated PickerState
    | TonePositionUpdated PickerState
    | JSEventReceived Ports.JSEvent
    | AppStateChangedInactive Time.Posix
    | AppStateChangedActive Time.Posix


splashTimer : Float
splashTimer =
    4000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        tempTimer =
            model.tempTimer
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Load ->
            ( { model | loaded = True }
            , Process.sleep splashTimer
                |> Task.perform
                    (\_ -> Start)
            )

        Start ->
            ( { model | screen = Countdown Normal }
            , Ports.storage "flags"
                (JE.object [ ( "lastScreen", JE.string "Countdown" ) ])
                |> Ports.fromElm
            )

        ScreenSelected canDelete lastScreen screen ->
            let
                timers =
                    if canDelete then
                        List.filter
                            (\t -> t.state /= Timer.Deleted)
                            model.timers

                    else
                        model.timers
            in
            -- Sets temp timer to existing timer to edit, pauses that timer
            case ( lastScreen, screen ) of
                -- If on the same screen, don't reset
                ( TimerEdit _ _, TimerEdit _ _ ) ->
                    ( { model
                        | screen = screen
                        , lastScreen = lastScreen
                        , timers = timers
                      }
                    , Cmd.none
                    )

                ( _, TimerEdit id _ ) ->
                    let
                        target =
                            List.filter (.id >> (==) id) timers
                                |> List.head
                    in
                    case target of
                        Just target_ ->
                            target_
                                |> withTimer
                                    { model
                                        | screen = screen
                                        , lastScreen = lastScreen
                                        , startTimeWheels = setWheels target_.startTime
                                        , restTimeWheels = setPicker target_.restTime
                                    }

                        Nothing ->
                            ( { model | screen = lastScreen, timers = timers }, Cmd.none )

                _ ->
                    ( { model
                        | screen = screen
                        , lastScreen = lastScreen
                        , timers = timers
                      }
                    , Cmd.none
                    )

        Tick delta ->
            let
                ( timers, cmds ) =
                    List.map (tick delta) model.timers
                        |> List.unzip
            in
            ( { model
                | timers = timers
              }
            , Cmd.batch cmds
            )

        PauseAll ->
            let
                timers =
                    List.map
                        (\t ->
                            { t
                                | state =
                                    case t.state of
                                        Timer.Counting ->
                                            Timer.Paused

                                        Timer.RestCounting ->
                                            Timer.RestPaused

                                        other ->
                                            other
                            }
                        )
                        model.timers
            in
            ( { model
                | timers = timers
              }
            , Cmd.none
            )

        AlarmStopped id ->
            alarmStopped model id

        AlarmReset id ->
            let
                ( timers, cmds ) =
                    List.map
                        (\t ->
                            if t.id == id then
                                resetAlarm t

                            else
                                ( t, Cmd.none )
                        )
                        model.timers
                        |> List.unzip
            in
            ( { model | timers = timers }, Cmd.batch cmds )

        ResetAll ->
            let
                ( timers, cmds ) =
                    List.map resetAlarm model.timers
                        |> List.unzip
            in
            ( { model | timers = timers }
            , Cmd.batch cmds
            )

        PlayAll ->
            let
                ( timers, cmds ) =
                    List.map
                        (\t ->
                            case t.state of
                                Timer.Paused ->
                                    let
                                        timer =
                                            { t | state = Timer.Counting }
                                    in
                                    ( timer
                                    , Ports.alarmStarted timer |> Ports.fromElm
                                    )

                                Timer.RestPaused ->
                                    let
                                        timer =
                                            { t | state = Timer.RestCounting }
                                    in
                                    ( timer, Ports.alarmStarted timer |> Ports.fromElm )

                                other ->
                                    ( t, Cmd.none )
                        )
                        model.timers
                        |> List.unzip
            in
            ( { model | timers = timers }, Cmd.batch cmds )

        SwipeMsg id swipe ->
            let
                timers =
                    List.map
                        (\t ->
                            if id == t.id then
                                { t | swipe = swipe }

                            else
                                t
                        )
                        model.timers
            in
            ( { model | timers = timers }, Cmd.none )

        DeleteTimer id ->
            deleteTimer model id

        ToggleState id state ->
            let
                ( timers, cmds ) =
                    List.map
                        (\t ->
                            if t.id == id then
                                case state of
                                    Timer.Counting ->
                                        let
                                            timer =
                                                { t | state = state }
                                        in
                                        ( timer
                                        , if t.state == Timer.RestCounting || t.state == Timer.Counting then
                                            Cmd.none

                                          else
                                            Ports.alarmStarted timer |> Ports.fromElm
                                        )

                                    Timer.RestCounting ->
                                        let
                                            timer =
                                                { t | state = state }
                                        in
                                        ( timer
                                        , if t.state == Timer.Counting then
                                            Cmd.none

                                          else
                                            Ports.alarmStarted timer |> Ports.fromElm
                                        )

                                    Timer.Paused ->
                                        ( { t | state = state }
                                        , Ports.alarmStopped t |> Ports.fromElm
                                        )

                                    Timer.RestPaused ->
                                        ( { t | state = state }
                                        , Ports.alarmStopped t |> Ports.fromElm
                                        )

                                    Timer.Deleted ->
                                        ( { t | state = state }
                                        , Ports.alarmStopped t |> Ports.fromElm
                                        )

                                    _ ->
                                        ( t, Cmd.none )

                            else
                                ( t, Cmd.none )
                        )
                        model.timers
                        |> List.unzip
            in
            ( { model | timers = timers }, Cmd.batch cmds )

        MomentumStart inx y ->
            case
                List.drop inx
                    model.startTimeWheels
                    |> List.head
                    |> Maybe.map (momentumStart y)
            of
                Just task ->
                    ( model, Task.perform (WheelUpdated inx) task )

                Nothing ->
                    ( model, Cmd.none )

        DragStep inx y ->
            case
                List.drop inx
                    model.startTimeWheels
                    |> List.head
                    |> Maybe.map (dragStep y)
            of
                Just task ->
                    ( model, Task.perform (WheelUpdated inx) task )

                Nothing ->
                    ( model, Cmd.none )

        MomentumStop inx ->
            let
                wheels =
                    List.indexedMap
                        (\i w ->
                            if i == inx then
                                { w | decelerating = True }

                            else
                                w
                        )
                        model.startTimeWheels
            in
            ( { model | startTimeWheels = wheels }, Cmd.none )

        WheelUpdated inx w ->
            let
                wheels =
                    List.indexedMap
                        (\i ws ->
                            if i == inx then
                                w

                            else
                                ws
                        )
                        model.startTimeWheels
            in
            ( { model | startTimeWheels = wheels }, Cmd.none )

        MomentumStep diff ->
            let
                wheels =
                    List.map
                        (\w ->
                            if w.decelerating then
                                momentumStep diff w

                            else
                                w
                        )
                        model.startTimeWheels
            in
            ( { model | startTimeWheels = wheels }, Cmd.none )

        ColorSelected c ->
            { tempTimer | color = c }
                |> withTimer model

        InputName name ->
            { tempTimer | label = name }
                |> withTimer model

        Done ->
            let
                startTime =
                    case model.startTimeWheels of
                        [ hours, minutes, seconds ] ->
                            List.sum
                                [ hours.value * 60 * 60
                                , minutes.value * 60
                                , seconds.value
                                ]
                                * 1000
                                |> toFloat

                        _ ->
                            0

                timer =
                    { state = Timer.Paused
                    , label = tempTimer.label
                    , startTime = startTime
                    , currentTime = startTime
                    , tone = tempTimer.tone
                    , vibrate = tempTimer.vibrate
                    , repetitions = tempTimer.repetitions
                    , currentRepetition = 1
                    , restTime = tempTimer.restTime
                    , currentRestTime = tempTimer.restTime
                    , color = tempTimer.color
                    , swipe = Swipe.init
                    , id = model.nextId
                    }
            in
            if startTime == 0 then
                ( model, Cmd.none )

            else if String.trim model.tempTimer.label == "" then
                ( model, Cmd.none )

            else
                case model.screen of
                    TimerEdit id _ ->
                        ( { model
                            | timers =
                                List.map
                                    (\t ->
                                        if t.id == id then
                                            { tempTimer
                                                | currentTime =
                                                    if t.startTime == startTime then
                                                        t.currentTime

                                                    else
                                                        startTime
                                                , startTime = startTime
                                            }

                                        else
                                            t
                                    )
                                    model.timers
                            , screen = model.lastScreen
                            , startTimeWheels = initWheel
                            , restTimeWheels = List.repeat 3 initPickerState
                            , tempTimer = Timer.init
                            , tonePickerPosition =
                                { initPickerState
                                    | index = 1
                                    , currentPosition = -32
                                }
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( { model
                            | screen = model.lastScreen
                            , timers = timer :: model.timers --List.repeat 100 timer
                            , nextId = model.nextId + 1
                            , startTimeWheels = initWheel
                            , restTimeWheels = List.repeat 3 initPickerState
                            , tempTimer = Timer.init
                            , tonePickerPosition =
                                { initPickerState
                                    | index = 1
                                    , currentPosition = -32
                                }
                            , repetitionPickerPosition = initPickerState
                          }
                        , Cmd.none
                        )

        InputVibrate vibrate ->
            { tempTimer | vibrate = vibrate }
                |> withTimer model

        RestWheelUpdated inx state ->
            let
                restTimeWheels =
                    List.indexedMap
                        (\i s ->
                            if i == inx then
                                state

                            else
                                s
                        )
                        model.restTimeWheels
            in
            ( { model | restTimeWheels = restTimeWheels }, Cmd.none )

        ToneSelected tone ->
            { tempTimer | tone = tone }
                |> withTimer model

        RepetitionModalOpened isOpen ->
            ( { model | repetitionModalOpen = isOpen }, Cmd.none )

        ToneModalOpened isOpen ->
            ( { model | toneModalOpen = isOpen }, Cmd.none )

        RestTimeModalOpened isOpen ->
            if isOpen then
                ( { model | restTimeModalOpen = isOpen }, Cmd.none )

            else
                let
                    rt =
                        case model.restTimeWheels of
                            [ hours, minutes, seconds ] ->
                                List.sum
                                    [ hours.index * 60 * 60
                                    , minutes.index * 60
                                    , seconds.index
                                    ]
                                    * 1000
                                    |> toFloat

                            _ ->
                                0
                in
                { tempTimer | restTime = rt }
                    |> withTimer { model | restTimeModalOpen = isOpen }

        RepetitionPositionUpdated picker_ ->
            { tempTimer | repetitions = picker_.index + 1 }
                |> withTimer { model | repetitionPickerPosition = picker_ }

        TonePositionUpdated picker_ ->
            { tempTimer
                | tone =
                    List.drop picker_.index Timer.alarmTones
                        |> List.head
                        |> Maybe.withDefault Timer.Beep
            }
                |> withTimer { model | tonePickerPosition = picker_ }

        JSEventReceived jsEvent ->
            case jsEvent of
                Ports.NotificationClosed { id } ->
                    alarmStopped model id

                Ports.AppStateChanged { isActive } ->
                    appStateChanged model isActive

                Ports.BackgroundTimerUpdates bgUpdates ->
                    let
                        timers_ =
                            List.foldl
                                (\bgUpdate acc ->
                                    List.map
                                        (\t ->
                                            if t.id == bgUpdate.id then
                                                { t
                                                    | state = bgUpdate.state
                                                    , currentTime = bgUpdate.currentTime
                                                    , currentRestTime = bgUpdate.currentRestTime
                                                    , currentRepetition = bgUpdate.currentRepetition
                                                }

                                            else
                                                t
                                        )
                                        acc
                                )
                                model.timers
                                bgUpdates.timers
                    in
                    ( { model | timers = timers_ }, Cmd.none )

        AppStateChangedInactive _ ->
            ( model, Cmd.none )

        AppStateChangedActive _ ->
            ( model, Cmd.none )


alarmStopped : Model -> Int -> ( Model, Cmd Msg )
alarmStopped model id =
    let
        ( timers, cmds ) =
            List.map
                (\t ->
                    if t.id == id && t.state == Timer.Ringing then
                        if t.repetitions > 0 && t.currentRepetition < t.repetitions then
                            let
                                ( currentRepetition, state ) =
                                    if t.restTime > 0 then
                                        ( t.currentRepetition, Timer.RestCounting )

                                    else
                                        ( t.currentRepetition + 1, Timer.Counting )
                            in
                            ( { t
                                | state = state
                                , currentRepetition = currentRepetition
                                , currentTime = t.startTime
                                , currentRestTime = t.restTime
                              }
                            , Ports.fromElm (Ports.stopRing t)
                            )

                        else
                            ( { t | state = Timer.Reset }, Ports.fromElm (Ports.stopRing t) )

                    else
                        ( t, Cmd.none )
                )
                model.timers
                |> List.unzip
    in
    ( { model | timers = timers }, Cmd.batch cmds )



-- Unsure if this is the way to go, might be better to create
-- alarms and cancel them on pause / play


appStateChanged : Model -> Bool -> ( Model, Cmd Msg )
appStateChanged model isActive =
    if isActive then
        ( model, Ports.fromElm Ports.nowActive )

    else
        ( model, Time.now |> Task.perform AppStateChangedInactive )


deleteTimer : Model -> Int -> ( Model, Cmd Msg )
deleteTimer model id =
    let
        ( timers, cmds ) =
            List.map
                (\t ->
                    if t.id == id then
                        ( { t | state = Timer.Deleted }
                        , case t.state of
                            Timer.Ringing ->
                                Ports.stopRing t
                                    |> Ports.fromElm

                            Timer.Counting ->
                                Ports.alarmStopped t
                                    |> Ports.fromElm

                            Timer.RestCounting ->
                                Ports.alarmStopped t
                                    |> Ports.fromElm

                            _ ->
                                Cmd.none
                        )

                    else
                        ( t, Cmd.none )
                )
                model.timers
                |> List.unzip

        screen =
            case model.screen of
                TimerEdit _ _ ->
                    model.lastScreen

                s ->
                    s
    in
    if List.all (.state >> (==) Timer.Deleted) timers then
        ( { model | timers = [], screen = screen }, Cmd.batch cmds )

    else
        ( { model | timers = timers, screen = screen }, Cmd.batch cmds )


resetAlarm : Timer -> ( Timer, Cmd Msg )
resetAlarm t =
    let
        init_ =
            Timer.init

        cmd =
            case t.state of
                Timer.Counting ->
                    Ports.alarmStopped t |> Ports.fromElm

                Timer.RestCounting ->
                    Ports.alarmStopped t |> Ports.fromElm

                _ ->
                    Cmd.none
    in
    ( { init_
        | label = t.label
        , startTime = t.startTime
        , currentTime = t.startTime
        , currentRestTime = t.currentRestTime
        , restTime = t.restTime
        , tone = t.tone
        , vibrate = t.vibrate
        , repetitions = t.repetitions
        , color = t.color
        , id = t.id
      }
    , cmd
    )


withTimer : Model -> Timer -> ( Model, Cmd Msg )
withTimer model t =
    ( { model | tempTimer = t }, Cmd.none )


tick : Float -> Timer -> ( Timer, Cmd Msg )
tick delta timer =
    case timer.state of
        Timer.Counting ->
            let
                time =
                    timer.currentTime - delta

                state =
                    if timer.restTime > 0 then
                        Timer.RestCounting

                    else
                        Timer.Counting
            in
            if time <= 0 then
                if timer.repetitions > 1 && timer.currentRepetition < timer.repetitions then
                    ( { timer
                        | currentTime = 0
                        , state = state
                      }
                    , Cmd.batch
                        [ if timer.vibrate then
                            Ports.hapticVibrate 500
                                |> Ports.fromElm

                          else
                            Cmd.none
                        ]
                    )

                else
                    ( { timer | currentTime = 0, state = Timer.Ringing }
                    , Ports.fromElm (Ports.ring timer)
                    )

            else
                ( { timer | currentTime = time }, Cmd.none )

        Timer.RestCounting ->
            let
                time =
                    timer.currentRestTime - delta
            in
            if time <= 0 then
                ( { timer
                    | state = Timer.Counting
                    , currentTime = timer.startTime
                    , currentRestTime = timer.restTime
                    , currentRepetition = timer.currentRepetition + 1
                  }
                , if timer.vibrate then
                    Ports.hapticVibrate 250
                        |> Ports.fromElm

                  else
                    Cmd.none
                )

            else
                ( { timer | currentRestTime = time }, Cmd.none )

        _ ->
            ( timer, Cmd.none )



-- VIEW


styles =
    CG.global
        [ CG.body
            [ Css.padding Css.zero
            , Css.margin Css.zero
            ]
        , CG.selector "body"
            [ Css.property "scrollbar-width" "none"
            , Css.property "-ms-overflow-style" "none"
            , Css.important (Css.property "overflow" "auto")
            ]
        , CG.selector "body::-webkit-scrollbar"
            [ Css.display Css.none ]
        , CG.selector "*, *::before, *::after"
            [ Css.boxSizing Css.borderBox ]
        ]


sequenceDurations : List Float
sequenceDurations =
    [ 5, 2, 9, 9, 4, 8, 3, 6, 2, 3, 4, 9, 5, 2, 3, 2, 4, 3 ]


colors : List ( Int, Timer.Color )
colors =
    [ Timer.salmon
    , Timer.orange
    , Timer.gold
    , Timer.gold
    , Timer.lightBlue
    , Timer.orange
    , Timer.purple
    , Timer.orange
    , Timer.purple
    , Timer.gold
    , Timer.lightBlue
    , Timer.salmon
    , Timer.salmon
    , Timer.purple
    , Timer.gold
    , Timer.gold
    , Timer.orange
    , Timer.salmon
    ]
        |> List.indexedMap Tuple.pair


view : Model -> Html Msg
view model =
    Html.div
        []
        [ styles
        , Html.node "link"
            [ Attributes.attribute "crossorigin" "", Attributes.href "https://fonts.gstatic.com", Attributes.rel "preconnect" ]
            []
        , Html.node "link"
            [ Attributes.href "https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400&display=swap", Attributes.rel "stylesheet" ]
            []
        , topMenu model
        , Html.div [ css [ Css.paddingTop (Css.px 56) ] ] []
        , case model.screen of
            Thumbnail mode ->
                if List.isEmpty model.timers then
                    noTimers

                else
                    thumbnailScreen mode model
                        |> bottomMenuSpacer

            Countdown mode ->
                if List.isEmpty model.timers then
                    noTimers

                else
                    countdownScreen mode model
                        |> bottomMenuSpacer

            TimerAdd type_ ->
                timerAdd type_ model

            TimerEdit id type_ ->
                timerEdit id type_ model

            Splash ->
                splashScreen model.loaded

            Credits ->
                creditScreen
        , bottomMenu model
        ]


creditScreen : Html msg
creditScreen =
    Html.div
        [ css
            [ Css.textAlign Css.center
            ]
        ]
        [ Html.h2 [] [ Html.text "UX Design", Html.br [] [], Html.text "Alyssa" ]
        , Html.hr [] []
        , Html.h2 [] [ Html.text "Programming", Html.br [] [], Html.text "Mike" ]
        ]


splashScreen : Bool -> Html Msg
splashScreen loaded =
    let
        opacity =
            if loaded then
                1

            else
                0

        title =
            Html.h1
                [ css
                    [ Css.position Css.absolute
                    , Css.zIndex (Css.int 3)
                    , Css.color (Css.hex "fff")
                    , Css.textAlign Css.center
                    , Css.top (Css.vw 100)
                    , Css.fontWeight (Css.int 400)
                    , Css.width (Css.pct 100)
                    ]
                ]
                [ Html.text "Multitimer" ]

        cube duration ( inx, { lighter, darker } ) =
            if inx == 7 then
                Html.img
                    [ css
                        [ Css.width (Css.pct 33.333)
                        , Css.height (Css.vw 33.333)
                        , Css.backgroundColor (Css.hex "fff")
                        , Css.transform (Css.scale 1.0)
                        ]
                    , Attributes.src "splashCenter.png"
                    ]
                    []

            else
                Html.div
                    [ css
                        [ Css.width (Css.pct 33.333)
                        , Css.height (Css.vw 33.333)
                        , Css.backgroundColor (Css.hex darker)
                        , Css.animationDuration (Css.ms <| duration * 150)
                        , Css.property "animation-iteration-count" "infinite"
                        , Animations.keyframes
                            [ ( 0, [ Animations.backgroundColor (Css.hex darker) ] )
                            , ( 50, [ Animations.backgroundColor (Css.hex lighter) ] )
                            ]
                            |> Css.animationName
                        ]
                    ]
                    []
    in
    List.map2 cube
        sequenceDurations
        colors
        |> (\stuff ->
                Html.div
                    [ css
                        [ Css.height (Css.vh 100)
                        , Css.width (Css.vw 100)
                        , Css.position Css.absolute
                        , Css.top (Css.px 0)
                        , Css.backgroundColor (Css.hex "#fff")
                        , Css.zIndex (Css.int 1000)
                        , Css.overflow Css.hidden
                        , Css.displayFlex
                        , Css.justifyContent Css.flexStart
                        , Css.property "align-content" "flex-start"
                        , Css.flexWrap Css.wrap
                        , Css.opacity (Css.num opacity)
                        , Transitions.transition
                            [ Transitions.opacity (splashTimer - 1000) ]

                        --, Css.flexDirection Css.column
                        --, Css.justifyContent Css.center
                        --, Css.alignItems Css.center
                        ]
                    ]
                    (title :: stuff)
           )



{-
   let
       ( background, opacity ) =
           if model.loaded then
               ( Timer.lightBlue.darker, 1 )

           else
               ( "fff", 0 )
   in
   Html.div
       [ css
           [ Css.height (Css.vh 100)
           , Css.width (Css.vw 100)
           , Css.position Css.absolute
           , Css.top (Css.px 0)
           , Css.backgroundColor (Css.hex background)
           , Css.zIndex (Css.int 1000)
           , Css.displayFlex
           , Css.flexDirection Css.column
           , Css.justifyContent Css.center
           , Css.alignItems Css.center
           , Transitions.transition
               [ Transitions.backgroundColor (splashTimer - 1000)
               ]
           ]
       ]
       [ Html.h1
           [ css
               [ Transitions.transition
                   [ Transitions.opacity (splashTimer - 1000) ]
               , Css.opacity (Css.num opacity)
               , Css.color (Css.hex "fff")
               , Css.marginBottom (Css.px 10)
               ]
           ]
           [ Html.text "Multitimer" ]
       , Html.div
           []
           [ Html.span
               [ css
                   [ Css.color (Css.hex Timer.salmon.lighter)
                   , Css.padding2 (Css.px 0) (Css.px 5)
                   , Transitions.transition [ Transitions.opacity (splashTimer - 1500) ]
                   , Css.opacity (Css.num opacity)
                   ]
               ]
               [ Icon.alarmClock_ "none" 64 64 ]
           , Html.span
               [ css
                   [ Css.color (Css.hex Timer.gold.lighter)
                   , Css.padding2 (Css.px 0) (Css.px 5)
                   , Transitions.transition [ Transitions.opacity (splashTimer - 1500) ]
                   , Css.opacity (Css.num opacity)
                   ]
               ]
               [ Icon.alarmClock_ "none" 64 64 ]
           , Html.span
               [ css
                   [ Css.color (Css.hex Timer.purple.lighter)
                   , Css.padding2 (Css.px 0) (Css.px 5)
                   , Transitions.transition [ Transitions.opacity (splashTimer - 1500) ]
                   , Css.opacity (Css.num opacity)
                   ]
               ]
               [ Icon.alarmClock_ "none" 64 64 ]
           ]
       ]
-}


bottomMenuSpacer : Html Msg -> Html Msg
bottomMenuSpacer elem =
    Html.div
        [ css [ Css.paddingBottom (Css.px 69) ] ]
        [ elem ]


topMenu : Model -> Html Msg
topMenu model =
    let
        options =
            case model.screen of
                TimerAdd _ ->
                    editTopMenu model

                TimerEdit id type_ ->
                    editTopMenu model

                Countdown mode ->
                    case mode of
                        Normal ->
                            normalTopMenu Countdown model

                        Editing ->
                            editingModeTopMenu Countdown

                Thumbnail mode ->
                    case mode of
                        Normal ->
                            normalTopMenu Thumbnail model

                        Editing ->
                            editingModeTopMenu Thumbnail

                Credits ->
                    [ Html.div
                        [ Events.onClick (ScreenSelected False model.lastScreen model.lastScreen) ]
                        [ Html.text "Back" ]
                    ]

                _ ->
                    []
    in
    if List.isEmpty options then
        Html.text ""

    else
        Html.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                , Css.alignItems Css.center
                , Css.padding2 Css.zero (Css.px 10)
                , Css.height (Css.px 56)
                , Css.position Css.fixed
                , Css.width (Css.pct 100)
                , Css.top Css.zero
                , Css.zIndex (Css.int 99)
                , Css.backgroundColor (Css.hex "#fff")
                , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#CDCDCD")
                ]
            ]
            options


editingModeTopMenu : (Mode -> Screen) -> List (Html Msg)
editingModeTopMenu back =
    [ Html.div
        [ Events.onClick ResetAll ]
        [ Html.text "Reset All" ]
    , Html.div
        [ css [ Css.paddingRight (Css.px 6), Css.fontWeight Css.bold ]
        , Events.onClick <| ScreenSelected False (back Editing) (back Normal)
        ]
        [ Html.text "Cancel" ]
    ]


editTopMenu : Model -> List (Html Msg)
editTopMenu model =
    let
        startTime =
            List.map .value model.startTimeWheels
                |> List.sum

        done =
            if model.tempTimer.label == "" || startTime == 0 then
                Html.div
                    [ css
                        [ Css.paddingRight (Css.px 6)
                        , Css.fontWeight Css.bold
                        , Css.opacity (Css.num 0.3)
                        ]
                    ]
                    [ Html.text "Done" ]

            else
                Html.div
                    [ css [ Css.paddingRight (Css.px 6), Css.fontWeight Css.bold ]
                    , Events.onClick Done
                    ]
                    [ Html.text "Done" ]
    in
    [ Html.div
        [ Events.onClick (ScreenSelected True model.screen model.lastScreen)
        ]
        [ Html.text "Cancel" ]
    , done
    ]


normalTopMenu : (Mode -> Screen) -> Model -> List (Html Msg)
normalTopMenu back model =
    if List.isEmpty model.timers then
        [ Html.div [ Events.onClick <| ScreenSelected False (back Normal) Credits ] [ Html.text "Credits" ] ]

    else
        [ Html.div
            [ Events.onClick ResetAll ]
            [ Html.text "Reset All" ]
        , Html.div
            [ css [ Css.paddingRight (Css.px 6), Css.fontWeight Css.bold ]
            , Events.onClick <| ScreenSelected False (back Normal) (back Editing)
            ]
            [ Html.text "Edit" ]
        ]


bottomMenu : Model -> Html Msg
bottomMenu model =
    case model.screen of
        Thumbnail _ ->
            bottomMenu_
                [ bottomPlayPause model
                , menuIcon_ (ScreenSelected True model.screen <| Countdown Normal) Icon.view
                , menuIcon_ (ScreenSelected True model.screen <| TimerAdd Timer.Simple) Icon.plus
                ]

        Countdown _ ->
            bottomMenu_
                [ bottomPlayPause model
                , menuIcon_ (ScreenSelected True model.screen <| Thumbnail Normal) Icon.grid
                , menuIcon_ (ScreenSelected True model.screen <| TimerAdd Timer.Simple) Icon.plus
                ]

        _ ->
            Html.text ""


bottomMenu_ : List (Html msg) -> Html msg
bottomMenu_ menu =
    Html.div
        [ css
            [ Css.position Css.fixed
            , Css.bottom (Css.px 0)
            , Css.left (Css.px 0)
            , Css.height (Css.px 69)
            , Css.width (Css.pct 100)
            , Css.zIndex (Css.int 99)
            , Css.backgroundColor (Css.hex "#fff")
            , Css.color (Css.hex "#5D5D5D")
            ]
        ]
        [ Html.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                , Css.padding2 (Css.px 20) (Css.px 20)
                , Css.borderTop3 (Css.px 0.5) Css.solid (Css.hex "D0D0D0")
                ]
            ]
            menu
        ]


bottomPlayPause : Model -> Html Msg
bottomPlayPause model =
    if List.any (.state >> (==) Timer.Counting) model.timers then
        Icon.pause_ "#5D5D5D" 24 24
            |> menuIcon_ PauseAll

    else
        menuIcon_ PlayAll Icon.play


menuIcon_ : msg -> Html msg -> Html msg
menuIcon_ event icon =
    Html.div [ Events.onClick event ] [ icon ]


noTimers : Html Msg
noTimers =
    Html.div
        [ css
            [ Css.backgroundColor (Css.hex "6D90A6")
            , Css.color (Css.hex "607B8D")
            , Css.width (Css.vw 100)
            , Css.height (Css.vh 90)
            , Css.displayFlex
            , Css.justifyContent Css.center
            , Css.alignItems Css.center
            , Css.fontSize (Css.px 24)
            , Css.textAlign Css.center
            ]
        ]
        [ Html.div
            [ css
                [ Css.width (Css.pct 40)
                ]
            ]
            [ Html.text "Click +", Html.br [] [], Html.text "to make your first timer" ]
        ]


thumbnailScreen : Mode -> Model -> Html Msg
thumbnailScreen mode model =
    List.map (thumbnail mode) model.timers
        |> Html.div
            [ css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                , Css.alignItems Css.center
                , Css.maxWidth (Css.px 372)
                , Css.marginLeft Css.auto
                , Css.marginRight Css.auto
                ]
            ]


thumbnail : Mode -> Timer -> Html Msg
thumbnail mode timer =
    let
        type_ =
            if timer.repetitions > 1 || timer.restTime > 0 then
                Timer.Interval

            else
                Timer.Simple

        ( onClick, background ) =
            case mode of
                Normal ->
                    ( Events.onClick NoOp
                    , Css.hex timer.color.darker
                    )

                Editing ->
                    ( TimerEdit timer.id type_
                        |> ScreenSelected True (Thumbnail Editing)
                        |> Events.onClick
                    , Css.hex timer.color.lighter
                    )

        displayTime =
            case timer.state of
                Timer.Counting ->
                    millisToDigital timer.currentTime

                Timer.Paused ->
                    millisToDigital timer.currentTime

                Timer.RestPaused ->
                    millisToDigital timer.currentRestTime

                Timer.RestCounting ->
                    millisToDigital timer.currentRestTime

                _ ->
                    ""
    in
    case timer.state of
        Timer.Deleted ->
            Html.text ""

        _ ->
            Html.div
                [ css
                    [ Css.width (Css.px 117)
                    , Css.display Css.inlineBlock
                    , Css.height (Css.px 117)
                    , Css.margin (Css.px 3)
                    , Css.color (Css.hex "#fff")
                    , Css.backgroundColor background
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.center
                    , Css.flexDirection Css.column
                    ]
                , onClick
                ]
                [ case mode of
                    Editing ->
                        Icon.chevronRight_ 24 24
                            |> flip thumbIcon NoOp

                    Normal ->
                        case timer.state of
                            Timer.Counting ->
                                ToggleState timer.id Timer.Paused
                                    |> thumbIcon Icon.pause

                            Timer.Paused ->
                                ToggleState timer.id Timer.Counting
                                    |> thumbIcon Icon.play

                            Timer.Ringing ->
                                AlarmStopped timer.id
                                    |> thumbIcon (Icon.alarmClock_ "none" 24 24)

                            Timer.RestCounting ->
                                ToggleState timer.id Timer.RestPaused
                                    |> thumbIcon (Icon.moon_ "none" 24 24)

                            Timer.RestPaused ->
                                ToggleState timer.id Timer.RestCounting
                                    |> thumbIcon Icon.play

                            Timer.Reset ->
                                AlarmReset timer.id
                                    |> thumbIcon (Icon.reset_ "none" 24 24)

                            _ ->
                                Html.text ""
                , Html.div
                    [ css
                        [ Css.fontSize (Css.px 18)
                        ]
                    ]
                    [ Html.text timer.label ]
                , Html.div
                    [ css
                        [ Css.fontSize (Css.px 22)
                        , Css.fontWeight (Css.int 200)
                        ]
                    ]
                    [ Html.text displayTime ]
                ]


swipeConfig : Swipe.Config
swipeConfig =
    let
        c =
            Swipe.defaultConfig
    in
    { c
        | swipeDirection = Swipe.left
        , closeOnLeftSwipe = True
        , leftSwipePercentage = 40
        , leftSwipeBackground =
            Html.div
                [ css
                    [ Css.backgroundColor (Css.hex "#B92E2E")
                    , Css.width (Css.pct 100)
                    , Css.height (Css.pct 100)
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.flexEnd
                    , Css.color (Css.hex "#fff")
                    , Css.padding (Css.rem 2)
                    ]
                ]
                [ Icon.trashcan_ "none" 48 48
                ]
                |> Html.map never
                |> Html.toUnstyled
    }


millisToHuman : Float -> String
millisToHuman t =
    let
        hours =
            t / (3600 * 1000) |> floor

        minutes =
            t / (60 * 1000) |> floor

        seconds =
            t / 1000 |> floor
    in
    if hours > 0 then
        String.fromInt hours ++ "hr"

    else if minutes > 0 then
        String.fromInt minutes ++ "min"

    else
        String.fromInt seconds ++ "sec"


millisToDigital : Float -> String
millisToDigital t =
    let
        time =
            t
                / 1000
                |> ceiling
    in
    if time >= 0 then
        let
            seconds =
                modBy 60 time
                    |> String.fromInt
                    |> String.padLeft 2 '0'

            minutes =
                time
                    // 60
                    |> modBy 60
                    |> String.fromInt
                    |> String.padLeft 2 '0'

            hours =
                time
                    // 3600
                    |> String.fromInt
                    |> String.padLeft 2 '0'
        in
        hours ++ ":" ++ minutes ++ ":" ++ seconds

    else
        ""


countdownScreen : Mode -> Model -> Html Msg
countdownScreen mode model =
    List.map (countdownListItem mode) model.timers
        |> Html.div []


countdownListItem : Mode -> Timer -> Html Msg
countdownListItem mode timer =
    let
        type_ =
            if timer.repetitions > 1 || timer.restTime > 0 then
                Timer.Interval

            else
                Timer.Simple

        time =
            case timer.state of
                Timer.RestCounting ->
                    millisToDigital timer.currentRestTime

                Timer.RestPaused ->
                    millisToDigital timer.currentRestTime

                _ ->
                    millisToDigital timer.currentTime

        reps =
            if timer.repetitions > 1 then
                "Reps: "
                    ++ String.fromInt timer.currentRepetition
                    ++ "/"
                    ++ String.fromInt timer.repetitions

            else
                ""

        ( percentage, icon ) =
            case mode of
                Normal ->
                    ( (timer.startTime - timer.currentTime)
                        / timer.startTime
                        * 100
                    , case timer.state of
                        Timer.Counting ->
                            countdownListItemIcon Icon.pause_ (ToggleState timer.id Timer.Paused)

                        Timer.Paused ->
                            countdownListItemIcon Icon.play_ (ToggleState timer.id Timer.Counting)

                        Timer.Ringing ->
                            countdownListItemIcon (always Icon.alarm) (AlarmStopped timer.id)

                        Timer.RestCounting ->
                            countdownListItemIcon (\_ w h -> Icon.moon_ "none" w h) (ToggleState timer.id Timer.RestPaused)

                        Timer.RestPaused ->
                            countdownListItemIcon Icon.play_ (ToggleState timer.id Timer.RestCounting)

                        Timer.Reset ->
                            countdownListItemIcon Icon.reset_ (AlarmReset timer.id)

                        _ ->
                            countdownListItemIcon Icon.play_ NoOp
                    )

                Editing ->
                    TimerEdit timer.id type_
                        |> ScreenSelected True (Countdown Editing)
                        |> countdownListItemIcon (always Icon.chevronRight_)
                        |> Tuple.pair 100

        evt =
            case mode of
                Normal ->
                    NoOp

                Editing ->
                    ScreenSelected True (Countdown Editing) <| TimerEdit timer.id type_
    in
    Html.div
        [ css
            [ Css.width (Css.vw 100)
            , Css.height (Css.px 152)
            , Css.position Css.relative
            ]
        ]
        [ Html.div
            [ css
                [ Css.position Css.absolute
                , Css.height (Css.pct 100)
                , Css.width (Css.vw 100)
                , Css.backgroundColor (Css.hex timer.color.darker)
                , Css.zIndex (Css.int 1)
                ]
            ]
            []
        , Html.div
            [ css
                [ Css.position Css.absolute
                , Css.height (Css.pct 100)
                , Css.width (Css.vw percentage)
                , Transitions.transition [ Transitions.width3 0.25 0 Transitions.ease ]
                , Css.backgroundColor (Css.hex timer.color.lighter)
                , Css.zIndex (Css.int 2)
                ]
            ]
            []
        , Html.div
            [ css
                [ Css.position Css.absolute
                , Css.padding2 (Css.px 26) (Css.px 32)
                , Css.height (Css.pct 100)
                , Css.width (Css.pct 100)
                , Css.zIndex (Css.int 3)
                , Css.color (Css.hex "fff")
                , Css.displayFlex
                , Css.flexDirection Css.column
                ]
            ]
            [ Html.h3
                [ css
                    [ Css.padding Css.zero
                    , Css.margin Css.zero
                    , Css.marginLeft (Css.px 2)
                    ]
                ]
                [ Html.text timer.label ]
            , Html.p
                [ css
                    [ Css.fontSize (Css.px 48)
                    , Css.fontWeight (Css.int 200)
                    , Css.margin Css.zero
                    , Css.lineHeight (Css.int 1)
                    ]
                ]
                [ Html.text time ]
            ]
        , icon
        , Html.div
            [ css
                [ Css.position Css.absolute
                , Css.bottom (Css.px 20)
                , Css.left (Css.px 38)
                , Css.zIndex (Css.int 3)
                , Css.color (Css.hex "fff")
                ]
            ]
            [ Html.text reps
            , if timer.restTime > 0 then
                " \u{00A0} Rest: "
                    ++ millisToHuman timer.restTime
                    |> Html.text

              else
                Html.text ""
            ]
        ]
        |> Html.toUnstyled
        |> Swipe.customSwipable swipeConfig
            [ ElementEvents.onClick evt ]
            (always <| DeleteTimer timer.id)
            (SwipeMsg timer.id)
            timer.swipe
        |> Html.fromUnstyled


countdownListItemIcon : (String -> Float -> Float -> Html msg) -> msg -> Html msg
countdownListItemIcon icon event =
    Html.div
        [ css
            [ Css.position Css.absolute
            , Css.top (Css.pct 33)
            , Css.right (Css.px 32)
            , Css.zIndex (Css.int 3)
            , Css.color (Css.hex "#fff")
            , Css.height (Css.pct 33)
            ]
        , Events.onClick event
        ]
        [ icon "#fff" 48 48 ]


timerEdit : Int -> Timer.TimerType -> Model -> Html Msg
timerEdit id type_ model =
    let
        timerTypeSelected =
            TimerEdit id
                >> ScreenSelected True model.lastScreen
    in
    timerEditScreen True id timerTypeSelected type_ model


timerAdd : Timer.TimerType -> Model -> Html Msg
timerAdd type_ model =
    let
        timerTypeSelected =
            TimerAdd
                >> ScreenSelected True model.lastScreen
    in
    timerEditScreen False -1 timerTypeSelected type_ model


timerEditScreen : Bool -> Int -> (Timer.TimerType -> Msg) -> Timer.TimerType -> Model -> Html Msg
timerEditScreen showDelete inx onTimer timerType m =
    Html.div
        [ css
            [ Css.backgroundColor (Css.hex "#EFEFEF")
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.minHeight (Css.calc (Css.vh 100) Css.minus (Css.px 56))
            ]
        ]
        [ input InputName m.tempTimer.label
        , colorPicker ColorSelected m.tempTimer.color
        , segmentedControl onTimer timerType
        , wheelSelector 26 MomentumStart DragStep MomentumStop m.startTimeWheels
        , multitimerSettings
            { onCheck = InputVibrate
            , checked = m.tempTimer.vibrate
            , repetitions = m.tempTimer.repetitions
            , restTime = m.tempTimer.restTime
            , onToneSelected = ToneSelected
            , tone = m.tempTimer.tone
            , timerType = timerType
            }
        , if showDelete then
            deleteOption (DeleteTimer inx)

          else
            Html.text ""
        , modal
            (RepetitionModalOpened False)
            "Repetitions"
            (picker (always RepetitionPositionUpdated) [ ( m.repetitionPickerPosition, repetitionOptions ) ])
            m.repetitionModalOpen
        , modal (ToneModalOpened False)
            "Alarm Tone"
            (picker (always TonePositionUpdated) [ ( m.tonePickerPosition, List.map Timer.toneToString Timer.alarmTones ) ])
            m.toneModalOpen
        , modal
            (RestTimeModalOpened False)
            "Rest Time"
            (List.map2 Tuple.pair m.restTimeWheels restTimeOptions
                |> picker RestWheelUpdated
            )
            m.restTimeModalOpen
        ]


restTimeOptions : List (List String)
restTimeOptions =
    let
        val n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n
    in
    [ List.range 0 99
        |> List.map val
    , List.range 0 59
        |> List.map val
    , List.range 0 59
        |> List.map val
    ]


repetitionOptions : List String
repetitionOptions =
    List.range 1 100
        |> List.map String.fromInt


type alias MultitimerSettings msg =
    { onCheck : Bool -> msg
    , checked : Bool
    , repetitions : Int
    , restTime : Float
    , onToneSelected : Timer.AlarmTone -> msg
    , tone : Timer.AlarmTone
    , timerType : Timer.TimerType
    }



-- TODO when changing to simple, set repetitions and rest time to 0


multitimerSettings : MultitimerSettings Msg -> Html Msg
multitimerSettings m =
    let
        extraSettings =
            case m.timerType of
                Timer.Simple ->
                    []

                Timer.Interval ->
                    [ listItem (RepetitionModalOpened True) (String.fromInt m.repetitions) "Repetitions"
                    , listItem (RestTimeModalOpened True) (millisToDigital m.restTime) "Rest Time"
                    ]
    in
    Html.div
        []
        [ Html.ul
            [ class "list" ]
          <|
            extraSettings
                ++ [ listItem (ToneModalOpened True) "Tone" (Timer.toneToString m.tone)
                   , Html.li [ class "list-item" ]
                        [ Html.div [ class "list-item__center" ] [ Html.text "Vibrate" ]
                        , Html.div
                            [ class "list-item__right"
                            , css
                                [ Css.padding Css.zero
                                , Css.paddingRight (Css.px 12)
                                ]
                            ]
                            [ switch m.onCheck m.checked ]
                        ]
                   ]
        ]


modal : msg -> String -> Html msg -> Bool -> Html msg
modal onClose title element open =
    Html.div
        [ css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.backgroundColor (Css.rgba 0 0 0 0.6)
            , Css.position Css.absolute
            , Css.top (Css.px 0)
            , Css.left (Css.px 0)
            , Css.zIndex (Css.int 100)
            , if open then
                Css.display Css.block

              else
                Css.display Css.none
            ]
        ]
        [ Html.div
            [ css
                [ Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.position Css.absolute
                , Css.top (Css.px 0)
                , Css.left (Css.px 0)
                ]
            , Events.onClick onClose
            ]
            []
        , Html.div
            [ css
                [ Css.zIndex (Css.int 102)
                , Css.position Css.absolute
                , Css.bottom (Css.pct 20)
                , Css.width (Css.vw 90)
                , Css.height (Css.pct 35)
                , Css.left (Css.vw 5)
                , Css.backgroundColor (Css.hex "#fff")
                , Css.padding (Css.px 10)
                ]
            ]
            [ Html.div
                []
                [ Html.h3
                    [ css
                        [ Css.marginTop Css.zero
                        , Css.marginBottom Css.zero
                        , Css.fontSize (Css.px 16)
                        , Css.fontWeight (Css.int 400)
                        , Css.fontFamilies [ "Roboto" ]
                        , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#D8D8D8")
                        , Css.paddingBottom (Css.px 10)
                        , Css.paddingLeft (Css.px 13)
                        , Css.paddingRight (Css.px 13)
                        ]
                    ]
                    [ Html.text title
                    , Html.span
                        [ css [ Css.property "float" "right", Css.color (Css.hex "#006ee6") ]
                        , Events.onClick onClose
                        ]
                        [ Html.text "Done" ]
                    ]
                ]
            , element
            ]
        ]


type alias PickerState =
    { startPosition : Float
    , currentPosition : Float
    , index : Int
    }


initPickerState : PickerState
initPickerState =
    { startPosition = 0
    , currentPosition = 0
    , index = 0
    }


picker : (Int -> PickerState -> msg) -> List ( PickerState, List String ) -> Html msg
picker updatePosition items =
    let
        selectionHeight =
            32

        offset_ =
            toFloat (selectionHeight * 2 + selectionHeight // 2)
    in
    Html.div
        []
        [ Html.div
            [ css
                [ Css.minHeight (Css.em 2)
                , Css.maxHeight (Css.em 2)
                , Css.backgroundColor (Css.rgba 150 150 150 0.1)
                , Css.width (Css.pct 100)
                , Css.top (Css.px <| offset_ + 40)
                , Css.borderRadius (Css.px 8)
                , Css.position Css.absolute
                ]
            ]
            []
        , List.indexedMap (\i ( state, options ) -> selectionSet (updatePosition i) options state) items
            |> Html.div
                [ css
                    [ Css.position Css.relative
                    , Css.height (Css.px 192)
                    , Css.overflow Css.hidden
                    , Css.displayFlex
                    , Css.justifyContent Css.center
                    ]
                ]
        ]


selectionSet : (PickerState -> msg) -> List String -> PickerState -> Html msg
selectionSet updatePosition options { startPosition, currentPosition, index } =
    let
        total =
            List.length options

        selectionHeight =
            32

        offset_ =
            toFloat (selectionHeight * 2 + selectionHeight // 2)

        index_ =
            if currentPosition >= 0 then
                0

            else if abs currentPosition >= selectionHeight * toFloat total then
                total

            else
                getValue selectionHeight total currentPosition - 1
    in
    Html.div
        [ css
            [ Css.position Css.relative
            , Css.top (Css.px (currentPosition + offset_))
            , Css.textAlign Css.center
            ]
        , JD.map
            (\b ->
                updatePosition
                    { currentPosition = currentPosition
                    , startPosition = b - currentPosition
                    , index = index
                    }
            )
            position
            |> Events.on "touchstart"
        , JD.map
            (\c ->
                let
                    b =
                        c - startPosition

                    b_ =
                        if b > selectionHeight then
                            selectionHeight

                        else if b < -(selectionHeight * toFloat total) then
                            -(selectionHeight * toFloat total)

                        else
                            b
                in
                ( updatePosition
                    { currentPosition = b_
                    , startPosition = startPosition
                    , index = index_
                    }
                , True
                )
            )
            position
            |> Events.preventDefaultOn "touchmove"
        , JD.map
            (\_ ->
                let
                    p =
                        if currentPosition > 0 then
                            0

                        else if abs currentPosition >= selectionHeight * (toFloat total - 1) then
                            negate (selectionHeight * (toFloat total - 1))

                        else
                            snapIntoPlace selectionHeight currentPosition
                in
                updatePosition
                    { currentPosition = p
                    , startPosition = 0
                    , index = getValue selectionHeight total p - 1
                    }
            )
            (JD.succeed ())
            |> Events.on "touchend"
        ]
        [ List.indexedMap
            (\i n ->
                let
                    color_ =
                        if i == index then
                            Css.color (Css.hex "000")

                        else
                            Css.color (Css.hex "B7B7B7")
                in
                Html.li
                    [ css
                        [ Css.fontSize (Css.px 24)
                        , Css.fontFamilies [ "Roboto", "sans-serif" ]
                        , Css.listStyle Css.none
                        , Css.lineHeight (Css.px 32)
                        , Css.height (Css.px 32)
                        , if List.length options > 1 then
                            Css.padding2 (Css.px 0) (Css.px 13)

                          else
                            Css.padding (Css.px 0)
                        , Css.property "transition-timing-function" "cubic-bezier(0.165, 0.84, 0.44, 1)"
                        , Css.property "transition-duration" "300ms"
                        , Css.property "transition-property" "transform"

                        --, trans
                        , color_
                        ]
                    ]
                    [ Html.text n ]
            )
            options
            |> Html.ul
                [ css
                    [ Css.marginTop Css.zero
                    , Css.padding Css.zero

                    --, Css.width (Css.px 52)
                    ]
                ]
        ]


listItem : msg -> String -> String -> Html msg
listItem onClick label value =
    Html.li
        [ class "list-item list-item--chevron"
        , Events.onClick onClick
        ]
        [ Html.div [ class "list-item__center" ] [ Html.text value ]
        , Html.div [ class "list-item__right list-item--chevron__right" ]
            [ Html.div [ class "list-item__label" ] [ Html.text label ]
            ]
        ]


deleteOption : msg -> Html msg
deleteOption onClick =
    Html.div
        [ css
            [ Css.marginTop (Css.px 26)
            , Css.backgroundColor (Css.hex "#fff")
            , Css.textAlign Css.center
            , Css.color (Css.hex "#FF4D43")
            , Css.lineHeight (Css.px 42)
            ]
        , Events.onClick onClick
        ]
        [ Html.text "Delete" ]


switch : (Bool -> msg) -> Bool -> Html msg
switch onCheck checked =
    Html.label
        [ Attributes.class "switch"
        ]
        [ Html.input
            [ Attributes.type_ "checkbox"
            , Attributes.class "switch__input"
            , Attributes.checked checked
            , Events.onCheck onCheck
            ]
            []
        , Html.div
            [ Attributes.class "switch__toggle"
            ]
            [ Html.div
                [ Attributes.class "switch__handle"
                ]
                []
            ]
        ]



-- Input


input : (String -> msg) -> String -> Html msg
input onInput name =
    Html.div
        [ css
            [ Css.width (Css.pct 100)
            , Css.paddingTop (Css.px 26)
            , Css.position Css.relative
            ]
        ]
        [ Html.div
            [ css
                [ Css.position Css.absolute
                , Css.right (Css.px 15)
                , Css.top (Css.px 35)
                , Css.color (Css.hex "#fff")
                ]
            , Events.onClick (onInput "")
            ]
            [ Icon.close_ "#8A8A8E" 24 24 ]
        , Html.input
            [ Attributes.placeholder "Name"
            , Attributes.value name
            , Events.onInput onInput
            , css
                [ Css.lineHeight (Css.px 32)
                , Css.fontSize (Css.px 16)
                , Css.width (Css.pct 100)
                , Css.display Css.block
                , Css.padding (Css.px 5)
                , Css.paddingLeft (Css.rem 1)
                , Css.property "border" "none"
                , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#CDCDCD")
                , Css.focus
                    [ Css.property "border" "none"
                    , Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "#CDCDCD")
                    , Css.property "outline" "none"
                    ]
                ]
            ]
            []
        ]


colorPicker : (Timer.Color -> msg) -> Timer.Color -> Html msg
colorPicker colorSelected currentColor =
    List.map (color colorSelected currentColor) Timer.colors
        |> Html.div
            [ css
                [ Css.paddingTop (Css.px 26)
                , Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                ]
            ]


color : (Timer.Color -> msg) -> Timer.Color -> Timer.Color -> Html msg
color colorSelected currentColor color_ =
    Html.div
        [ css [ Css.position Css.relative ]
        , Events.onClick (colorSelected color_)
        ]
        [ if currentColor.darker == color_.darker then
            Html.div
                [ css
                    [ Css.position Css.absolute
                    , Css.top (Css.px -4)
                    , Css.left (Css.px 11)
                    , Css.backgroundColor (Css.hex "CBCFD2")
                    , Css.width (Css.px 34)
                    , Css.height (Css.px 34)
                    , Css.borderRadius (Css.pct 50)
                    , Css.zIndex (Css.int 1)
                    ]
                ]
                []

          else
            Html.text ""
        , Html.div
            [ css
                [ Css.width (Css.px 26)
                , Css.height (Css.px 26)
                , Css.backgroundColor (Css.hex color_.darker)
                , Css.borderRadius (Css.pct 50)
                , Css.marginLeft (Css.px 15)
                , Css.marginRight (Css.px 15)
                , Css.position Css.relative
                , Css.zIndex (Css.int 2)
                ]
            ]
            []
        ]


segmentedControl : (Timer.TimerType -> msg) -> Timer.TimerType -> Html msg
segmentedControl onClick type_ =
    let
        translation =
            case type_ of
                Timer.Simple ->
                    0

                Timer.Interval ->
                    100
    in
    Html.div
        [ css
            [ Css.marginLeft Css.auto
            , Css.marginRight Css.auto
            , Css.marginTop (Css.px 24)
            , Css.width (Css.pct 60)
            ]
        ]
        [ List.map (\opt -> segmentedOption (onClick opt) (Timer.timerTypeToString opt)) [ Timer.Simple, Timer.Interval ]
            |> (++)
                [ Html.span
                    [ css
                        [ Css.backgroundColor (Css.hex "#fff")
                        , Css.border3 (Css.px 0.5) Css.solid (Css.rgba 0 0 0 0.04)
                        , Css.property "box-shadow" "0 3px 8px 0 rgba(0,0,0,0.12), 0 3px 1px 0 rgba(0,0,0,0.04)"
                        , Css.borderRadius (Css.px 7)
                        , Css.property "grid-column" "1"
                        , Css.property "grid-row" "1"
                        , Css.zIndex (Css.int 2)
                        , Css.position Css.relative
                        , Css.property "will-change" "transform"
                        , Css.property "transition" "transform 0.2s ease"
                        , Css.transform (Css.translateX (Css.pct translation))
                        ]
                    ]
                    []
                ]
            |> Html.div
                [ css
                    [ Css.backgroundColor (Css.hex "E0E0E2")
                    , Css.borderRadius (Css.px 9)
                    , Css.margin Css.zero
                    , Css.padding (Css.px 2)
                    , Css.property "border" "none"
                    , Css.outline Css.none
                    , Css.property "display" "grid"
                    , Css.property "grid-auto-flow" "column"
                    , Css.property "grid-auto-columns" "1fl"
                    , Css.property "-webkit-user-select" "none"
                    , Css.property "-moz-user-select" "none"
                    , Css.property "-ms-user-select" "none"
                    , Css.property "user-select" "none"
                    ]
                ]
        ]


segmentedOption : msg -> String -> Html msg
segmentedOption onClick option =
    Html.div
        [ css
            [ Css.position Css.relative
            , Css.cursor Css.pointer
            , Css.firstOfType
                [ Css.property "grid-column" "1"
                , Css.property "grid-row" "1"
                , Css.boxShadow Css.none
                , CG.descendants
                    [ CG.selector "label::after, label::before"
                        [ Css.opacity Css.zero ]
                    ]
                ]
            ]
        ]
        [ Html.input
            [ Attributes.type_ "radio"
            , Attributes.value option
            , Attributes.name "segment"
            , css
                [ Css.position Css.absolute
                , Css.top (Css.px 0)
                , Css.left (Css.px 0)
                , Css.right (Css.px 0)
                , Css.bottom (Css.px 0)
                , Css.width (Css.pct 100)
                , Css.height (Css.pct 100)
                , Css.padding Css.zero
                , Css.margin Css.zero
                , Css.property "appearance" "none"
                , Css.outline Css.none
                , Css.property "border" "none"
                , Css.opacity Css.zero
                ]
            ]
            []
        , Html.label
            [ Attributes.for option
            , css
                [ Css.position Css.relative
                , Css.display Css.block
                , Css.textAlign Css.center
                , Css.padding2 (Css.px 3) (Css.vmin 6)
                , Css.backgroundColor (Css.rgba 255 255 255 0)
                , Css.fontWeight (Css.int 500)
                , Css.color (Css.hex "#000")
                , Css.fontSize (Css.px 14)
                , Css.after
                    [ Css.property "content" "''"
                    , Css.width (Css.px 1)
                    , Css.backgroundColor (Css.rgba 142 142 147 0.15)
                    , Css.position Css.absolute
                    , Css.top (Css.pct 14)
                    , Css.bottom (Css.pct 14)
                    , Css.borderRadius (Css.px 10)
                    , Css.property "will-change" "background"
                    , Css.property "transition" "background 0.2s ease"
                    , Css.property "transform" "translateX(0.5px)"
                    ]
                , Css.before
                    [ Css.property "content" "''"
                    , Css.width (Css.px 1)
                    , Css.backgroundColor (Css.rgba 142 142 147 0.15)
                    , Css.position Css.absolute
                    , Css.top (Css.pct 14)
                    , Css.bottom (Css.pct 14)
                    , Css.borderRadius (Css.px 10)
                    , Css.property "will-change" "background"
                    , Css.property "transition" "background 0.2s ease"
                    , Css.property "transform" "translateX(-0.5px)"
                    ]
                ]
            ]
            [ Html.span
                [ css
                    [ Css.display Css.block
                    , Css.position Css.relative
                    , Css.zIndex (Css.int 2)
                    , Css.property "transition" "all 0.2 ease"
                    , Css.property "-webkit-transition" "all 0.2 ease"
                    , Css.property "will-change" "transform"
                    ]
                , Events.onClick onClick
                ]
                [ Html.text option ]
            ]
        ]


wheelSelector : Float -> (Int -> Float -> msg) -> (Int -> Float -> msg) -> (Int -> msg) -> List WheelSelector -> Html msg
wheelSelector margin onStart onDrag onStop wheelSelectors =
    List.indexedMap (wheel onStart onDrag onStop) wheelSelectors
        |> Html.div
            [ css
                [ Css.margin (Css.px margin)
                , Css.displayFlex
                , Css.justifyContent Css.spaceBetween
                , Css.overflow Css.hidden
                ]
            ]


wheel : (Int -> Float -> msg) -> (Int -> Float -> msg) -> (Int -> msg) -> Int -> WheelSelector -> Html msg
wheel onStart onDrag onStop inx { currentPosition, maxNum, value } =
    let
        strip =
            List.range 0 maxNum
                |> List.map wheelNumber

        wheelNumber n =
            let
                val =
                    if n < 10 then
                        "0" ++ String.fromInt n

                    else
                        String.fromInt n
            in
            Html.div
                [ css
                    [ if value == n then
                        Css.color (Css.hex "#969494")

                      else
                        Css.property "" ""
                    , Css.height (Css.px itemHeight)
                    , Css.padding Css.zero
                    , Css.margin Css.zero
                    , Css.fontSize (Css.px itemHeight)
                    , Css.lineHeight (Css.px itemHeight)
                    ]
                ]
                [ Html.text val ]
    in
    Html.div
        [ css
            [ Css.height (Css.px 121)
            , Css.width (Css.px 94)
            , Css.borderRadius (Css.px 20)
            , Css.border3 (Css.px 1) Css.solid (Css.hex "EFEFEF")
            , Css.backgroundColor (Css.hex "#fff")
            , Css.textAlign Css.center
            , Css.property "font-family" "Roboto"
            , Css.fontWeight (Css.int 100)
            , Css.color (Css.hex "#D8D8D8")
            , Css.padding (Css.px 0)
            ]
        ]
        [ Html.div
            [ css
                [ Css.position Css.relative
                , Css.top (Css.px (currentPosition - offset))
                ]
            , Events.on "touchstart" (JD.map (onStart inx) position)
            , Events.preventDefaultOn "touchmove" (JD.map (onDrag inx) position |> JD.map (\a -> ( a, True )))
            , Events.on "touchend" (JD.succeed (onStop inx))
            , Events.on "Css.touchcancel" (JD.succeed (onStop inx))
            ]
            (strip ++ strip ++ strip)
        ]


thumbIcon : Html msg -> msg -> Html msg
thumbIcon icon event =
    Html.div
        [ css [ Css.padding (Css.px 1), Css.color (Css.hex "fff") ]
        , Events.onClick event
        ]
        [ icon ]



-- Physics
{- We can store velocity per wheel, bounds will be determined by the number of
   elements in the wheel multiplied by the height to get distance.

   We can play with a mass value
-}


type alias WheelSelector =
    { startY : Float
    , startTime : Float
    , lastVelocity : Velocity
    , currentPosition : Float
    , maxNum : Int
    , value : Int
    , decelerating : Bool
    }


nearEnd : Int -> Float -> Bool
nearEnd maxInt pos =
    if abs pos < 34.5 then
        True

    else if (52.0 * toFloat (maxInt * 3) - 34.5) - abs pos < 34.5 then
        True

    else if pos > 0 then
        True

    else
        False


snapIntoPlace : Float -> Float -> Float
snapIntoPlace height pos =
    let
        mod =
            pos
                |> round
                |> modBy (round height)
                |> toFloat

        half =
            height / 2
    in
    if mod < half then
        pos - mod

    else
        pos + (height - mod)


initWheelSelector : Int -> WheelSelector
initWheelSelector maxNum =
    let
        startingPosition =
            (itemHeight * toFloat maxNum)
                |> negate
    in
    { startY = 0
    , startTime = 0
    , lastVelocity = Up 0
    , currentPosition = startingPosition
    , maxNum = maxNum
    , decelerating = False
    , value = getValue itemHeight maxNum startingPosition
    }


position : JD.Decoder Float
position =
    JD.at [ "targetTouches", "0", "pageY" ] JD.float


momentumStart : Float -> WheelSelector -> Task x WheelSelector
momentumStart y ws =
    Time.now
        |> Task.map
            (\t ->
                { ws | startY = y, startTime = toFloat (Time.posixToMillis t) }
            )



{- User fully controls object -}


dragStep : Float -> WheelSelector -> Task x WheelSelector
dragStep y ws =
    let
        deltaD =
            y - ws.startY
    in
    Time.now
        |> Task.map
            (\posix ->
                let
                    t =
                        toFloat (Time.posixToMillis posix)

                    deltaT_ =
                        t - ws.startTime

                    v =
                        if deltaT_ == 0 then
                            0

                        else
                            abs (deltaD / deltaT_)

                    p =
                        ws.currentPosition + deltaD

                    ( velocity, pos ) =
                        if y > ws.startY then
                            if nearEnd ws.maxNum p then
                                ( Down v, p - 52.0 * toFloat ws.maxNum )

                            else
                                ( Down v, p )

                        else if nearEnd ws.maxNum p then
                            ( Up v, p + 52.0 * toFloat ws.maxNum )

                        else
                            ( Up v, p )
                in
                { ws
                    | lastVelocity = velocity
                    , currentPosition = pos
                    , value = getValue itemHeight ws.maxNum pos
                    , startTime = t
                    , startY = y
                }
            )


getValue : Float -> Int -> Float -> Int
getValue height maxNum pos =
    (abs pos + height)
        / height
        |> round
        |> remainderBy (maxNum + 1)



{- Object is decelerating, request animation frame will provide ms diffs between frames -}


momentumStep : Float -> WheelSelector -> WheelSelector
momentumStep t ws =
    let
        ( newVelocity, newPosition, v_ ) =
            case ws.lastVelocity of
                Up v ->
                    let
                        newV =
                            v - v * friction

                        p =
                            ws.currentPosition - v * t
                    in
                    if nearEnd ws.maxNum p then
                        ( Up newV
                        , p + 52.0 * toFloat ws.maxNum
                        , newV
                        )

                    else
                        ( Up newV
                        , p
                        , newV
                        )

                Down v ->
                    let
                        newV =
                            v - v * friction

                        p =
                            ws.currentPosition + v * t
                    in
                    if nearEnd ws.maxNum p then
                        ( Down newV
                        , p - 52.0 * toFloat ws.maxNum
                        , newV
                        )

                    else
                        ( Down newV
                        , p
                        , newV
                        )
    in
    if v_ <= stopThreshold then
        let
            pos =
                snapIntoPlace itemHeight newPosition
        in
        { ws
            | lastVelocity = Up 0
            , currentPosition = pos
            , decelerating = False
            , startTime = 0
            , startY = 0
            , value = getValue itemHeight ws.maxNum pos
        }

    else
        { ws
            | lastVelocity = newVelocity
            , currentPosition = newPosition
            , value = getValue itemHeight ws.maxNum newPosition
        }


type Velocity
    = Up Float
    | Down Float


itemHeight : Float
itemHeight =
    52


thirdHeight : Int
thirdHeight =
    itemHeight
        / 3
        |> round


portholeHeight : Float
portholeHeight =
    121


offset : Float
offset =
    ((portholeHeight - itemHeight) / 2)
        - itemHeight
        |> abs


stopThreshold : Float
stopThreshold =
    0.3


friction : Float
friction =
    0.2


setWheels : Float -> List WheelSelector
setWheels t =
    let
        time =
            if t == 0 then
                0

            else
                floor t // 1000

        thours =
            time // 3600

        tminutes =
            modBy 3600 time // 60

        tseconds =
            modBy 60 time

        ( h, m, s ) =
            case initWheel of
                h_ :: m_ :: s_ :: _ ->
                    ( h_, m_, s_ )

                _ ->
                    ( initWheelSelector 0, initWheelSelector 0, initWheelSelector 0 )

        hours =
            toFloat h.maxNum * itemHeight + toFloat thours * itemHeight

        minutes =
            toFloat m.maxNum * itemHeight + toFloat tminutes * itemHeight

        seconds =
            toFloat s.maxNum * itemHeight + toFloat tseconds * itemHeight
    in
    [ { h | currentPosition = -hours, value = thours }
    , { m | currentPosition = -minutes, value = tminutes }
    , { s | currentPosition = -seconds, value = tseconds }
    ]


setPicker : Float -> List PickerState
setPicker t =
    if t == 0 then
        List.repeat 3 initPickerState

    else
        let
            time =
                floor t // 1000

            thours =
                time // 3600

            tminutes =
                modBy 3600 time // 60

            tseconds =
                modBy 60 time

            hours =
                toFloat thours * 32

            minutes =
                toFloat tminutes * 32

            seconds =
                toFloat tseconds * 32
        in
        [ { initPickerState | currentPosition = -hours, index = thours }
        , { initPickerState | currentPosition = -minutes, index = tminutes }
        , { initPickerState | currentPosition = -seconds, index = tseconds }
        ]


fromActive : Float -> Timer -> Timer
fromActive elapsedTime timer =
    if elapsedTime == 0 then
        timer

    else
        case timer.state of
            Timer.Counting ->
                let
                    time =
                        max 0 (timer.currentTime - elapsedTime)
                in
                if time == 0 then
                    if timer.currentRepetition < timer.repetitions then
                        if timer.restTime > 0 then
                            fromActive
                                (max 0 <| elapsedTime - timer.currentTime)
                                { timer
                                    | currentTime = timer.startTime
                                    , currentRestTime = timer.restTime
                                    , state = Timer.RestCounting
                                }

                        else
                            fromActive
                                (max 0 <| elapsedTime - timer.currentTime)
                                { timer
                                    | currentTime = timer.startTime
                                    , currentRepetition = timer.currentRepetition + 1
                                }

                    else
                        { timer | currentTime = time, state = Timer.Reset }

                else
                    { timer | currentTime = time }

            Timer.RestCounting ->
                let
                    time =
                        max 0 (timer.currentRestTime - elapsedTime)
                in
                if time == 0 then
                    fromActive
                        (max 0 <| elapsedTime - timer.currentRestTime)
                        { timer
                            | currentRestTime = timer.restTime
                            , currentRepetition = timer.currentRepetition + 1
                            , currentTime = timer.startTime
                            , state = Timer.Counting
                        }

                else
                    { timer | currentRestTime = time }

            _ ->
                timer


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b
