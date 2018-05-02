module Test.API exposing (..)

import Expect
import Test exposing (..)
import Date exposing (..)
import DatePicker
    exposing
        ( DatePicker
        , Selection(..)
        , initCalendar
        , getFrom
        , getTo
        , getSelectedDate
        , isOpen
        , getMonth
        , getNextMonth
        , setDate
        , toggleCalendar
        , nextMonth
        , previousMonth
        , clearDates
        , cancelDates
        )


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
                            Date.fromString "2018-09-09" |> Result.withDefault (Date.fromTime 0)

                        newCalendar =
                            setDate date rangeCalendar
                    in
                        Expect.all
                            [ \c -> Expect.equal (dateCase getFrom c) (Date.toTime date)
                            , \c -> Expect.equal (dateCase getTo c) 0
                            , \c -> Expect.equal (dateCase getSelectedDate c) 0
                            ]
                            newCalendar
            , test "second setDate should change to" <|
                \_ ->
                    let
                        date1 =
                            Date.fromString "2018-09-09" |> Result.withDefault (Date.fromTime 0)

                        date2 =
                            Date.fromString "2018-09-10" |> Result.withDefault (Date.fromTime 0)

                        newCalendar1 =
                            setDate date1 rangeCalendar

                        newCalendar2 =
                            setDate date2 newCalendar1
                    in
                        Expect.all
                            [ \c -> Expect.equal (dateCase getFrom c) (Date.toTime date1)
                            , \c -> Expect.equal (dateCase getTo c) (Date.toTime date2)
                            , \c -> Expect.equal (dateCase getSelectedDate c) 0
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
                    let
                        newCalendar =
                            nextMonth rangeCalendar
                    in
                        Expect.equal (getMonth newCalendar) ( 2018, Feb, [] )
            , test "nextMonth - getNextMonth" <|
                \_ ->
                    let
                        newCalendar =
                            nextMonth rangeCalendar

                        ( _, newMonth, _ ) =
                            getNextMonth newCalendar
                    in
                        Expect.equal newMonth Mar
            , test "previousMonth - getMonth" <|
                \_ ->
                    let
                        newCalendar =
                            previousMonth rangeCalendar

                        ( newYear, newMonth, _ ) =
                            getMonth newCalendar
                    in
                        Expect.equal ( newYear, newMonth ) ( 2017, Dec )
            , test "previousMonth - getNextMonth" <|
                \_ ->
                    let
                        newCalendar =
                            previousMonth rangeCalendar

                        ( _, newMonth, _ ) =
                            getNextMonth newCalendar
                    in
                        Expect.equal newMonth Jan
            , test "clearDates" <|
                \_ ->
                    let
                        date =
                            Date.fromString "2018-09-09" |> Result.withDefault (Date.fromTime 0)

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
                            Date.fromString "2018-09-09" |> Result.withDefault (Date.fromTime 0)

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
                            Date.fromString "2018-09-09" |> Result.withDefault (Date.fromTime 0)

                        newCalendar =
                            setDate date singleCalendar
                    in
                        Expect.all
                            [ \c -> Expect.equal (dateCase getSelectedDate c) (Date.toTime date)
                            , \c -> Expect.equal (dateCase getFrom c) 0
                            , \c -> Expect.equal (dateCase getTo c) 0
                            ]
                            newCalendar
            , test "second setDate should change selectedDate again" <|
                \_ ->
                    let
                        date1 =
                            Date.fromString "2018-09-09" |> Result.withDefault (Date.fromTime 0)

                        date2 =
                            Date.fromString "2018-09-10" |> Result.withDefault (Date.fromTime 0)

                        newCalendar1 =
                            setDate date1 singleCalendar

                        newCalendar2 =
                            setDate date2 newCalendar1
                    in
                        Expect.all
                            [ \c -> Expect.equal (dateCase getSelectedDate c) (Date.toTime date2)
                            , \c -> Expect.equal (dateCase getFrom c) 0
                            , \c -> Expect.equal (dateCase getTo c) 0
                            ]
                            newCalendar2
            , test "clearDates" <|
                \_ ->
                    let
                        date =
                            Date.fromString "2018-09-09" |> Result.withDefault (Date.fromTime 0)

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
                            Date.fromString "2018-09-09" |> Result.withDefault (Date.fromTime 0)

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


dateCase : (DatePicker -> Maybe Date) -> DatePicker -> Float
dateCase func calendar =
    case func calendar of
        Just d ->
            Date.toTime d

        Nothing ->
            0
