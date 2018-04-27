module Test.View exposing (..)

import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, classes)
import Html exposing (Html)
import Date exposing (..)
import DatePicker
    exposing
        ( DatePicker
        , Selection(..)
        , Msg
        , initCalendar
        , showCalendar
        , defaultConfig
        , Config
        )


rangeCalendar : DatePicker
rangeCalendar =
    initCalendar Range


calendarHtml : Html Msg
calendarHtml =
    showCalendar rangeCalendar ( 2018, Jan, [] ) defaultConfig


customConfig : Config
customConfig =
    { defaultConfig | titleClass = "red", weekdayFormat = "dd" }


customCalendar : Html Msg
customCalendar =
    showCalendar rangeCalendar ( 2018, Jan, [] ) customConfig


suite : Test
suite =
    describe "Show Calendar"
        [ describe "Default Calendar"
            [ test "Default Html Title" <|
                \_ ->
                    calendarHtml
                        |> Query.fromHtml
                        |> Query.find [ tag "h1" ]
                        |> Query.has [ text "2018 Jan", classes [ "tc" ] ]
            , test "Default Html Days" <|
                \_ ->
                    calendarHtml
                        |> Query.fromHtml
                        |> Query.find [ tag "thead" ]
                        |> Query.has
                            [ text "Mon"
                            , text "Tue"
                            , text "Wed"
                            , text "Thu"
                            , text "Fri"
                            , text "Sat"
                            , text "Sun"
                            ]
            ]
        , describe "Custom Calendar"
            [ test "Custom Html Title" <|
                \_ ->
                    customCalendar
                        |> Query.fromHtml
                        |> Query.find [ tag "h1" ]
                        |> Query.has [ classes [ "red" ] ]
            , test "Custom Html Days" <|
                \_ ->
                    customCalendar
                        |> Query.fromHtml
                        |> Query.find [ tag "thead" ]
                        |> Query.has
                            [ text "M"
                            , text "Tu"
                            , text "W"
                            , text "Th"
                            , text "F"
                            , text "Sa"
                            , text "Su"
                            ]
            ]
        ]
