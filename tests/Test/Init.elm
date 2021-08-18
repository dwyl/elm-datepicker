module Test.Init exposing (..)

import DatePicker
    exposing
        ( DatePicker
        , Selection(..)
        , getFrom
        , getMonth
        , getTo
        , initCalendar
        , isOpen
        )
import Expect
import Test exposing (..)
import Time exposing (Month(..))


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
            \_ -> Expect.equal (getMonth rangeCalendar) ( 2018, Jan )
        ]
