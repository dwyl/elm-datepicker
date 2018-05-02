module Test.Init exposing (..)

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
        , isOpen
        , getMonth
        , getNextMonth
        )


rangeCalendar : DatePicker
rangeCalendar =
    initCalendar Range


suite : Test
suite =
    describe "initCalendar - default values"
        [ test "Default from" <|
            \_ -> Expect.equal (getFrom rangeCalendar) Nothing
        , test "Default to" <|
            \_ -> Expect.equal (getTo rangeCalendar) Nothing
        , test "Default isOpen" <|
            \_ -> Expect.equal (isOpen rangeCalendar) False
        , test "Default Month" <|
            \_ -> Expect.equal (getMonth rangeCalendar) ( 2018, Jan, [] )
        , test "Default nextMonth" <|
            \_ -> Expect.equal (getNextMonth rangeCalendar) ( 2018, Feb, [] )
        ]
