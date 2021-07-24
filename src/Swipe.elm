module Swipe exposing
    ( Config
    , State
    , Swipe(..)
    , both
    , customSwipable
    , defaultConfig
    , init
    , left
    , right
    , swipable
    , swipableLeft
    , swipableRight
    )

import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events as Events
import Json.Decode as JD



-- TODO Add swipe disable
-- Interfaces


swipable : (Swipe -> msg) -> (State -> msg) -> State -> Html msg -> Html msg
swipable onSwipe onUpdate state =
    customSwipable defaultConfig [] onSwipe onUpdate state


swipableLeft : msg -> (State -> msg) -> State -> Html msg -> Html msg
swipableLeft onSwipe onUpdate state =
    customSwipable
        { defaultConfig | swipeDirection = left }
        []
        (always onSwipe)
        onUpdate
        state


swipableRight : msg -> (State -> msg) -> State -> Html msg -> Html msg
swipableRight onSwipe onUpdate state =
    customSwipable
        { defaultConfig | swipeDirection = right }
        []
        (always onSwipe)
        onUpdate
        state


customSwipable : Config -> List (Html.Attribute msg) -> (Swipe -> msg) -> (State -> msg) -> State -> Html msg -> Html msg
customSwipable config providedAttrs onSwipe onUpdate (State state) content =
    let
        ( attrs, outerAttrs ) =
            case state.phase of
                NoPhase ->
                    ( [ onTouchStart onUpdate (State state)

                      --, onMouseStart onUpdate (State state)
                      ]
                    , []
                    )

                Started ->
                    ( [ onInitialTouchMove onUpdate (State state)

                      --, onInitialMouseMove onUpdate (State state)
                      ]
                    , []
                    )

                Moving ->
                    ( [ onTouchMove config onUpdate (State state)

                      --, onMouseMove config onUpdate (State state)
                      , onTouchEnd config onSwipe onUpdate (State state)

                      --, onMouseUp config onSwipe onUpdate (State state)
                      --, onMouseLeave config onSwipe onUpdate (State state)
                      --, style "touch-action" "none"
                      ]
                    , []
                    )

                EndingSwipe swipe ->
                    ( []
                    , [ style "max-height" "0"
                      , onSwipe swipe
                            |> JD.succeed
                            |> Events.on "transitionend"
                      ]
                    )

                _ ->
                    ( [ init
                            |> onUpdate
                            |> JD.succeed
                            |> Events.on "transitionend"
                      , style "transition" "left 0.3s ease"
                      ]
                    , []
                    )

        background =
            case state.phase of
                EndingLeft ->
                    config.leftSwipeBackground
                        |> Html.map (always (onUpdate (State state)))

                EndingRight ->
                    config.rightSwipeBackground
                        |> Html.map (always (onUpdate (State state)))

                _ ->
                    Html.map (always (onUpdate (State state))) <|
                        if state.position == 0 then
                            Html.text ""

                        else if state.position > 0 then
                            config.rightSwipeBackground

                        else
                            config.leftSwipeBackground
    in
    Html.div
        ([ style "padding" "0"
         , style "position" "relative"
         , style "overflow" "hidden"
         , style "max-height" "500px"
         , style "transition" "max-height 0.3s ease"
         ]
            ++ providedAttrs
            ++ outerAttrs
        )
        [ Html.div
            [ style "position" "absolute"
            , style "top" "0"
            , style "right" "0"
            , style "bottom" "0"
            , style "left" "0"
            , style "z-index" "1"
            ]
            [ background ]
        , Html.div
            ([ style "position" "relative"
             , style "backgroundColor" config.foregroundColor
             , style "z-index" "2"
             , style "-webkit-user-select" "none"
             , style "-moz-user-select" "none"
             , style "-ms-user-select" "none"
             , style "left" (String.fromFloat state.position ++ "px")
             ]
                ++ attrs
            )
            [ content ]
        ]



-- STATE


type State
    = State State_


type Phase
    = NoPhase
    | Started
    | Moving
    | EndingSwipe Swipe
    | EndingLeft
    | EndingRight


type alias State_ =
    { startX : Float
    , startY : Float
    , position : Float
    , phase : Phase
    }


init : State
init =
    State
        { startX = 0
        , startY = 0
        , position = 0
        , phase = NoPhase
        }



-- EVENTS


type Swipe
    = Left
    | Right


onTouchStart : (State -> msg) -> State -> Html.Attribute msg
onTouchStart onUpdate (State state) =
    JD.map2 (start_ state)
        (JD.at [ "targetTouches", "0", "pageX" ] JD.float)
        (JD.at [ "targetTouches", "0", "pageY" ] JD.float)
        |> JD.map onUpdate
        |> Events.on "touchstart"


onMouseStart : (State -> msg) -> State -> Html.Attribute msg
onMouseStart onUpdate (State state) =
    JD.map2 (start_ state)
        (JD.at [ "pageX" ] JD.float)
        (JD.at [ "pageY" ] JD.float)
        |> JD.map onUpdate
        |> Events.on "mousedown"


start_ : State_ -> Float -> Float -> State
start_ state x y =
    State
        { state
            | startX = x
            , startY = y
            , position = 0
            , phase = Started
        }


onInitialTouchMove : (State -> msg) -> State -> Html.Attribute msg
onInitialTouchMove onUpdate (State state) =
    JD.map2 (initialMove_ state)
        (JD.at [ "targetTouches", "0", "pageX" ] JD.float)
        (JD.at [ "targetTouches", "0", "pageY" ] JD.float)
        |> JD.map (Tuple.first >> onUpdate)
        |> Events.on "touchmove"


onInitialMouseMove : (State -> msg) -> State -> Html.Attribute msg
onInitialMouseMove onUpdate (State state) =
    JD.map2 (initialMove_ state)
        (JD.at [ "pageX" ] JD.float)
        (JD.at [ "pageY" ] JD.float)
        |> JD.map (Tuple.mapFirst onUpdate)
        |> Events.preventDefaultOn "mousemove"


initialMove_ : State_ -> Float -> Float -> ( State, Bool )
initialMove_ state x y =
    let
        dx =
            x - state.startX

        dy =
            y - state.startY
    in
    if abs dx > abs dy then
        ( State { state | phase = Moving }, False )

    else
        ( init, False )


onTouchMove : Config -> (State -> msg) -> State -> Html.Attribute msg
onTouchMove config onUpdate (State state) =
    JD.map
        (onMove_ config state)
        (JD.at [ "targetTouches", "0", "pageX" ] JD.float)
        |> JD.map (Tuple.first >> onUpdate)
        |> Events.on "touchmove"


onMouseMove : Config -> (State -> msg) -> State -> Html.Attribute msg
onMouseMove config onUpdate (State state) =
    JD.map
        (onMove_ config state)
        (JD.at [ "pageX" ] JD.float)
        |> JD.map (Tuple.mapFirst onUpdate)
        |> Events.preventDefaultOn "mousemove"


onMove_ : Config -> State_ -> Float -> ( State, Bool )
onMove_ config state x =
    let
        dx =
            x - state.startX
    in
    case config.swipeDirection of
        B ->
            ( State { state | position = dx }
            , True
            )

        L ->
            if dx < 0 then
                ( State { state | position = dx }
                , True
                )

            else
                ( init, False )

        R ->
            if dx > 0 then
                ( State { state | position = dx }
                , True
                )

            else
                ( init, False )


onTouchEnd : Config -> (Swipe -> msg) -> (State -> msg) -> State -> Html.Attribute msg
onTouchEnd config onSwipe onUpdate (State state) =
    JD.map2
        (onEnd_ config state onSwipe onUpdate)
        (JD.at [ "currentTarget", "offsetLeft" ] JD.float)
        (JD.at [ "currentTarget", "clientWidth" ] JD.float)
        |> Events.on "touchend"


onMouseUp : Config -> (Swipe -> msg) -> (State -> msg) -> State -> Html.Attribute msg
onMouseUp config onSwipe onUpdate (State state) =
    JD.map2
        (onEnd_ config state onSwipe onUpdate)
        (JD.at [ "currentTarget", "offsetLeft" ] JD.float)
        (JD.at [ "currentTarget", "clientWidth" ] JD.float)
        |> Events.on "mouseup"


onMouseLeave : Config -> (Swipe -> msg) -> (State -> msg) -> State -> Html.Attribute msg
onMouseLeave config onSwipe onUpdate (State state) =
    JD.map2
        (onEnd_ config state onSwipe onUpdate)
        (JD.at [ "currentTarget", "offsetLeft" ] JD.float)
        (JD.at [ "currentTarget", "clientWidth" ] JD.float)
        |> Events.on "mouseleave"


endPosition : State -> State
endPosition (State state) =
    if state.position == 0 then
        State { state | phase = NoPhase }

    else if state.position > 0 then
        State { state | phase = EndingRight, position = 0 }

    else
        State { state | phase = EndingLeft, position = 0 }


onEnd_ : Config -> State_ -> (Swipe -> msg) -> (State -> msg) -> Float -> Float -> msg
onEnd_ config state onSwipe onUpdate shift width =
    let
        pct =
            abs (shift / width) * 100
    in
    case config.swipeDirection of
        B ->
            if pct >= config.leftSwipePercentage && shift < 0 then
                if config.closeOnLeftSwipe then
                    State { state | phase = EndingSwipe Left }
                        |> onUpdate

                else
                    onSwipe Left

            else if pct >= config.rightSwipePercentage && shift > 0 then
                if config.closeOnRightSwipe then
                    State { state | phase = EndingSwipe Right }
                        |> onUpdate

                else
                    onSwipe Right

            else
                endPosition (State state)
                    |> onUpdate

        L ->
            if pct >= config.leftSwipePercentage && shift < 0 then
                if config.closeOnLeftSwipe then
                    State { state | phase = EndingSwipe Left }
                        |> onUpdate

                else
                    onSwipe Left

            else
                endPosition (State state)
                    |> onUpdate

        R ->
            if pct >= config.rightSwipePercentage && shift > 0 then
                if config.closeOnRightSwipe then
                    State { state | phase = EndingSwipe Right }
                        |> onUpdate

                else
                    onSwipe Right

            else
                endPosition (State state)
                    |> onUpdate



-- Config


type alias Config =
    { leftSwipeBackground : Html Never
    , rightSwipeBackground : Html Never
    , foregroundColor : String
    , leftSwipePercentage : Float
    , rightSwipePercentage : Float
    , swipeDirection : Direction
    , closeOnLeftSwipe : Bool
    , closeOnRightSwipe : Bool
    }


type Direction
    = B
    | L
    | R


both : Direction
both =
    B


left : Direction
left =
    L


right : Direction
right =
    R


defaultConfig : Config
defaultConfig =
    { leftSwipeBackground = leftSwipeBackground
    , rightSwipeBackground = rightSwipeBackground
    , closeOnLeftSwipe = False
    , closeOnRightSwipe = False
    , foregroundColor = "#ffffff"
    , leftSwipePercentage = 50
    , rightSwipePercentage = 50
    , swipeDirection = both
    }


leftSwipeBackground : Html Never
leftSwipeBackground =
    Html.div
        [ style "background-color" "red"
        , style "width" "100%"
        , style "height" "100%"
        ]
        []
        |> Html.map never


rightSwipeBackground : Html Never
rightSwipeBackground =
    Html.div
        [ style "background-color" "green"
        , style "width" "100%"
        , style "height" "100%"
        ]
        []
        |> Html.map never
