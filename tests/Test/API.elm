module Test.API exposing (..)

import Date exposing (Date)
import DatePicker
    exposing
        ( DatePicker
        , Selection(..)
        , cancelDates
        , clearDates
        , getFrom
        , getMonth
        , getSelectedDate
        , getTo
        , initCalendar
        , isOpen
        , nextMonth
        , previousMonth
        , setDate
        , setMonth
        , toggleCalendar
        )
import Expect
import Test exposing (..)
import Time exposing (Month(..))


rangeCalendar : DatePicker
rangeCalendar =
    initCalendar Range


singleCalendar : DatePicker
singleCalendar =
    initCalendar Single


suite : Test
suite =
    describe "API functions"
        [ describe "applied to rangeCalendar"
            [ test "setDate should change from" <|
                \_ ->
                    let
                        date =
                            parseDate "2018-09-09"

                        newCalendar =
                            setDate date rangeCalendar
                    in
                    Expect.all
                        [ \c -> Expect.equal (dateCase getFrom c) date
                        , \c -> Expect.equal (dateCase getTo c) invalid
                        , \c -> Expect.equal (dateCase getSelectedDate c) invalid
                        ]
                        newCalendar
            , test "second setDate should change to" <|
                \_ ->
                    let
                        date1 =
                            parseDate "2018-09-09"

                        date2 =
                            parseDate "2018-09-10"

                        newCalendar1 =
                            setDate date1 rangeCalendar

                        newCalendar2 =
                            setDate date2 newCalendar1
                    in
                    Expect.all
                        [ \c -> Expect.equal (dateCase getFrom c) date1
                        , \c -> Expect.equal (dateCase getTo c) date2
                        , \c -> Expect.equal (dateCase getSelectedDate c) invalid
                        ]
                        newCalendar2
            , test "toggleCalendar" <|
                \_ ->
                    let
                        newCalendar =
                            toggleCalendar rangeCalendar
                    in
                    Expect.equal (isOpen newCalendar) True
            , test "nextMonth - getMonth" <|
                \_ ->
                    Expect.equal (rangeCalendar |> nextMonth |> getMonth) ( 2018, Feb )
            , test "previousMonth - getMonth" <|
                \_ ->
                    Expect.equal (rangeCalendar |> previousMonth |> getMonth) ( 2017, Dec )
            , test "clearDates" <|
                \_ ->
                    let
                        date =
                            parseDate "2018-09-09"

                        newCalendar =
                            setDate date rangeCalendar

                        newCalendar2 =
                            clearDates newCalendar
                    in
                    Expect.all
                        [ \c -> Expect.equal (getFrom c) Nothing
                        , \c -> Expect.equal (getTo c) Nothing
                        , \c -> Expect.equal (getSelectedDate c) Nothing
                        ]
                        newCalendar2
            , test "cancelDates" <|
                \_ ->
                    let
                        date =
                            parseDate "2018-09-09"

                        newCalendar =
                            setDate date rangeCalendar

                        newCalendar2 =
                            toggleCalendar newCalendar

                        newCalendar3 =
                            cancelDates newCalendar2
                    in
                    Expect.all
                        [ \c -> Expect.equal (getFrom c) Nothing
                        , \c -> Expect.equal (getTo c) Nothing
                        , \c -> Expect.equal (getSelectedDate c) Nothing
                        , \c -> Expect.equal (isOpen c) False
                        ]
                        newCalendar3
            ]
        , describe "applied to singleCalendar"
            [ test "setDate should change selectedDate" <|
                \_ ->
                    let
                        date =
                            parseDate "2018-09-09"

                        newCalendar =
                            setDate date singleCalendar
                    in
                    Expect.all
                        [ \c -> Expect.equal (dateCase getSelectedDate c) date
                        , \c -> Expect.equal (dateCase getFrom c) invalid
                        , \c -> Expect.equal (dateCase getTo c) invalid
                        ]
                        newCalendar
            , test "second setDate should change selectedDate again" <|
                \_ ->
                    let
                        date1 =
                            parseDate "2018-09-09"

                        date2 =
                            parseDate "2018-09-10"

                        newCalendar1 =
                            setDate date1 singleCalendar

                        newCalendar2 =
                            setDate date2 newCalendar1
                    in
                    Expect.all
                        [ \c -> Expect.equal (dateCase getSelectedDate c) date2
                        , \c -> Expect.equal (dateCase getFrom c) invalid
                        , \c -> Expect.equal (dateCase getTo c) invalid
                        ]
                        newCalendar2
            , test "setDisplayMonth should update calendar's year and month" <|
                \_ ->
                    let
                        date =
                            parseDate "2021-08-13"

                        ( year, month ) =
                            singleCalendar
                                |> setMonth ( Date.year date, Date.month date )
                                |> getMonth
                    in
                    Expect.equal ( year, month ) ( 2021, Aug )
            , test "clearDates" <|
                \_ ->
                    let
                        date =
                            parseDate "2018-09-09"

                        newCalendar =
                            setDate date singleCalendar

                        newCalendar2 =
                            clearDates newCalendar
                    in
                    Expect.all
                        [ \c -> Expect.equal (getFrom c) Nothing
                        , \c -> Expect.equal (getTo c) Nothing
                        , \c -> Expect.equal (getSelectedDate c) Nothing
                        ]
                        newCalendar2
            , test "cancelDates" <|
                \_ ->
                    let
                        date =
                            parseDate "2018-09-09"

                        newCalendar =
                            setDate date singleCalendar

                        newCalendar2 =
                            toggleCalendar newCalendar

                        newCalendar3 =
                            cancelDates newCalendar2
                    in
                    Expect.all
                        [ \c -> Expect.equal (getFrom c) Nothing
                        , \c -> Expect.equal (getTo c) Nothing
                        , \c -> Expect.equal (getSelectedDate c) Nothing
                        , \c -> Expect.equal (isOpen c) False
                        ]
                        newCalendar3
            ]
        ]


parseDate : String -> Date.Date
parseDate isoDate =
    Date.fromIsoString isoDate
        |> Result.withDefault invalid


invalid : Date
invalid =
    Date.fromCalendarDate 1970 Jan 1


dateCase : (DatePicker -> Maybe Date) -> DatePicker -> Date
dateCase func calendar =
    func calendar
        |> Maybe.withDefault invalid
