module Icon exposing
    ( alarm
    , alarmClock_
    , chevronRight_
    , close_
    , grid
    , moon_
    , pause
    , pause_
    , play
    , play_
    , plus
    , reset_
    , trashcan
    , trashcan_
    , view
    )

import Css
import Css.Animations
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)


play : Svg msg
play =
    play_ "none" 24 24


play_ : String -> Float -> Float -> Svg msg
play_ fl h w =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox "0 0 24 24"
        , fill fl
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ polygon [ points "5 3 19 12 5 21 5 3" ] [] ]


trashcan : Svg msg
trashcan =
    trashcan_ "none" 24 24


trashcan_ : String -> Float -> Float -> Svg msg
trashcan_ fl h w =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox "0 0 24 24"
        , fill fl
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-trash-2"
        ]
        [ polyline [ points "3 6 5 6 21 6" ] []
        , Svg.path [ d "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2" ] []
        , line [ x1 "10", y1 "11", x2 "10", y2 "17" ] []
        , line [ x1 "14", y1 "11", x2 "14", y2 "17" ] []
        ]


pause : Svg msg
pause =
    pause_ "#fff" 24 24


pause_ : String -> Float -> Float -> Svg msg
pause_ fl h w =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox "0 0 24 24"
        , fill fl
        , stroke "currentColor"
        , strokeWidth "1"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ rect [ x "6", y "4", width "1", height "16" ] []
        , rect [ x "14", y "4", width "1", height "16" ] []
        ]


grid : Svg msg
grid =
    svg
        [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ rect [ x "3", y "3", width "7", height "7" ] []
        , rect [ x "14", y "3", width "7", height "7" ] []
        , rect [ x "14", y "14", width "7", height "7" ] []
        , rect [ x "3", y "14", width "7", height "7" ] []
        ]


view : Svg msg
view =
    svg
        [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ rect [ x "2", y "2", width "20", height "8", rx "2", ry "2" ] []
        , rect [ x "2", y "14", width "20", height "8", rx "2", ry "2" ] []
        ]


plus : Svg msg
plus =
    svg
        [ width "24"
        , height "24"
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


close_ : String -> Float -> Float -> Svg msg
close_ fl h w =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox "0 0 24 24"
        , fill fl
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-x-circle"
        ]
        [ circle [ cx "12", cy "12", r "10" ] []
        , line [ x1 "15", y1 "9", x2 "9", y2 "15" ] []
        , line [ x1 "9", y1 "9", x2 "15", y2 "15" ] []
        ]


chevronRight_ : Float -> Float -> Svg msg
chevronRight_ h w =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-chevron-right"
        ]
        [ polyline [ points "9 18 15 12 9 6" ] [] ]


alarm : Float -> Float -> Html msg
alarm h w =
    Html.img
        [ Attributes.css
            [ Css.width (Css.px <| w + 25)
            , Css.height (Css.px <| h + 25)
            , Css.property "transform" "rotateZ(-15deg)"
            , Css.Animations.keyframes
                [ ( 0, [ Css.Animations.transform [ Css.rotateZ (Css.deg -19) ] ] )
                , ( 50
                  , [ Css.Animations.transform [ Css.rotateZ (Css.deg -11) ]

                    --, Css.Animations.opacity (Css.num 0.5)
                    ]
                  )
                ]
                |> Css.animationName
            , Css.animationDuration (Css.ms 250)
            , Css.property "animation-iteration-count" "infinite"
            , Css.position Css.relative
            , Css.bottom (Css.px 14)
            , Css.left (Css.px 15)
            ]
        , Attributes.src "./ringing.png"
        ]
        []


reset_ : String -> Float -> Float -> Html msg
reset_ fl h w =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ polyline [ points "1 4 1 10 7 10" ] [], Svg.path [ d "M3.51 15a9 9 0 1 0 2.13-9.36L1 10" ] [] ]


moon_ : String -> Float -> Float -> Html msg
moon_ fl h w =
    svg
        [ width (String.fromFloat w)
        , height (String.fromFloat h)
        , viewBox "0 0 24 24"
        , fill fl
        , stroke "currentColor"
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class "feather feather-moon"
        ]
        [ Svg.path [ d "M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z" ] [] ]


alarmClock_ : String -> Float -> Float -> Html msg
alarmClock_ fl h w =
    svg
        [ version "1.1"
        , fill "currentColor"
        , viewBox "0 0 512.002 512.002"
        , width (String.fromFloat w)
        , height (String.fromFloat h)
        , Svg.Styled.Attributes.style "enable-background:new 0 0 512.002 512.002;"
        ]
        [ g []
            [ g [] [ Svg.path [ d "M256.001,77.017c-107.656,0-195.244,87.589-195.244,195.244c0,107.662,87.589,195.25,195.244,195.25\n            c107.662,0,195.244-87.589,195.244-195.25C451.245,164.606,363.657,77.017,256.001,77.017z M256.001,432.126\n                        c-88.143,0-159.853-71.715-159.853-159.858s71.709-159.853,159.853-159.853s159.853,71.71,159.853,159.853\n                                    C415.854,360.416,344.144,432.126,256.001,432.126z" ] [] ] ]
        , g [] [ g [] [ Svg.path [ d "M310.268,266.363H263.08v-68.424c0-9.774-7.922-17.696-17.696-17.696c-9.774,0-17.696,7.922-17.696,17.696v86.12\n                                                c0,9.774,7.922,17.696,17.696,17.696h64.885c9.774,0,17.696-7.922,17.696-17.696C327.964,274.285,320.042,266.363,310.268,266.363\n                                                            z" ] [] ] ]
        , g [] [ g [] [ Svg.path [ d "M155.766,398.911c-7.267-6.542-18.457-5.946-24.992,1.315l-53.088,58.986c-6.542,7.261-5.946,18.451,1.315,24.987\n                                                                        c3.38,3.05,7.615,4.548,11.833,4.548c4.843,0,9.668-1.976,13.16-5.863l53.088-58.986\n                                                                                    C163.623,416.636,163.027,405.446,155.766,398.911z" ] [] ] ]
        , g [] [ g [] [ Svg.path [ d "M434.322,459.218l-53.088-58.986c-6.524-7.267-17.719-7.857-24.987-1.315c-7.267,6.536-7.851,17.725-1.315,24.987\n                                                                                                l53.088,58.986c3.486,3.881,8.311,5.857,13.154,5.857c4.212,0,8.447-1.498,11.833-4.542\n                                                                                                            C440.274,477.669,440.858,466.479,434.322,459.218z" ] [] ] ]
        , g [] [ g [] [ Svg.path [ d "M152.764,49.046c-35.162-34.43-91.841-34.377-126.342,0.13C9.256,66.335-0.123,89.039,0.001,113.105\n                                                                                                                        c0.13,23.777,9.556,46.039,26.55,62.685c3.445,3.368,7.91,5.049,12.381,5.049c4.53,0,9.06-1.734,12.511-5.179L152.899,74.204\n                                                                                                                                    c3.339-3.344,5.203-7.881,5.179-12.605C158.055,56.873,156.143,52.355,152.764,49.046z M40.873,136.174\n                                                                                                                                                c-3.545-7.143-5.439-15.047-5.48-23.258c-0.077-14.534,5.621-28.29,16.05-38.719c10.405-10.399,24.161-15.596,37.993-15.596\n                                                                                                                                                            c8.087,0,16.204,1.775,23.683,5.326L40.873,136.174z" ] [] ] ]
        , g [] [ g [] [ Svg.path [ d "M485.581,49.17c-34.507-34.501-91.187-34.56-126.348-0.13c-3.374,3.309-5.291,7.828-5.315,12.552\n                                                                                                                                                                        c-0.024,4.725,1.846,9.267,5.185,12.605l101.456,101.456c3.451,3.457,7.981,5.185,12.511,5.185c4.471,0,8.942-1.681,12.381-5.061\n                                                                                                                                                                                    c17-16.64,26.426-38.901,26.55-62.679C512.125,89.039,502.74,66.335,485.581,49.17z M471.13,136.174l-72.246-72.246\n                                                                                                                                                                                                c20.279-9.627,45.189-6.211,61.676,10.275c10.429,10.429,16.127,24.178,16.05,38.719\n                                                                                                                                                                                                            C476.568,121.127,474.675,129.037,471.13,136.174z" ] [] ] ]
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        , g [] []
        ]
