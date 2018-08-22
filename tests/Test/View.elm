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


customWeekdayFormatter : String -> Day -> String
customWeekdayFormatter _ day =
    case day of
        Mon ->
            "Mandag"

        Tue ->
            "Tirsdag"

        Wed ->
            "Onsdag"

        Thu ->
            "Torsdag"

        Fri ->
            "Fredag"

        Sat ->
            "Lørdag"

        Sun ->
            "Søndag"


customTitleFormatter : Int -> Month -> String
customTitleFormatter year month =
    let
        formattedMonth =
            case month of
                Jan ->
                    "januar"

                _ ->
                    "unknown"
    in
        formattedMonth ++ " " ++ toString year


customConfigWithFormatters : Config
customConfigWithFormatters =
    { defaultConfig
        | weekdayFormatter = customWeekdayFormatter
        , titleFormatter = customTitleFormatter
    }


customCalendar : Html Msg
customCalendar =
    showCalendar rangeCalendar ( 2018, Jan, [] ) customConfig


customCalenderWithFormatters : Html Msg
customCalenderWithFormatters =
    showCalendar rangeCalendar ( 2018, Jan, [] ) customConfigWithFormatters


suite : Test
suite =
    describe "Show Calendar"
        [ describe "Default Calendar"
            [ test "Default Html Title" <|
                \_ ->
                    calendarHtml
                        |> Query.fromHtml
                        |> Query.find [ tag "h1" ]
                        |> Query.has [ text "Jan 2018", classes [ "tc" ] ]
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
        , describe "Custom Calendar with Formatters"
            [ test "Formatted Html Title" <|
                \_ ->
                    customCalenderWithFormatters
                        |> Query.fromHtml
                        |> Query.find [ tag "h1" ]
                        |> Query.has [ text "januar 2018" ]
            , test "Formatted  Html Days" <|
                \_ ->
                    customCalenderWithFormatters
                        |> Query.fromHtml
                        |> Query.find [ tag "thead" ]
                        |> Query.has
                            [ text "Mandag"
                            , text "Tirsdag"
                            , text "Onsdag"
                            , text "Torsdag"
                            , text "Fredag"
                            , text "Lørdag"
                            , text "Søndag"
                            ]
            ]
        ]
