module DatePicker
    exposing
        ( DatePicker
        , Msg
        , Config
        , initCalendar
        , showCalendar
        , defaultConfig
        , update
        , Selection(..)
        , getFrom
        , getTo
        , isOpen
        , getMonth
        , getNextMonth
        , getSelectedDate
        , clearDates
        , toggleCalendar
        , cancelDates
        , previousMonth
        , nextMonth
        , receiveDate
        , setDate
        )

{-| A customisable DatePicker that easily allows you to select a range of dates

@docs DatePicker, Msg, Config, defaultConfig, initCalendar, showCalendar, update, Selection, receiveDate


# Getter Functions

These functions allow you to access data from the DatePicker model.

@docs getFrom, getTo, getMonth, getNextMonth, isOpen, getSelectedDate


# API Functions

These functions allow us to perform updates to the datepicker model. Just pass the datepicker as the last argument.
For example:

```Elm
  newDatepicker = DatePicker.setDate date datepicker
```

Or

```Elm
  newDatePicker = DatePicker.clearDates datepicker
```

@docs clearDates, toggleCalendar, cancelDates, previousMonth, nextMonth, setDate

-}

import Html.Events exposing (onClick, onInput, onMouseOver)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events.Extra exposing (onEnter)
import DateCore exposing (..)
import Date exposing (..)
import Task


type alias MonthData =
    ( Year, Month, List (Maybe Date) )


type FromTo
    = From
    | To
    | Only


{-| Type to pass to [`initCalendar`](#initCalendar) that will determine if your datepicker will
be used to select a single date or a range of dates
-}
type Selection
    = Single
    | Range


type alias Model =
    { currentDate : Maybe Date
    , month : MonthData
    , nextMonth : MonthData
    , open : Bool
    , selectDate : FromTo
    , from : Maybe Date
    , to : Maybe Date
    , single : Maybe Date
    , overDate : Maybe Date
    }


{-| The DatePicker model
-}
type DatePicker
    = DatePicker Model


{-| Opaque DatePicker Msg type
-}
type Msg
    = ReceiveDate Date
    | ToggleCalendar
    | PreviousMonth
    | NextMonth
    | SelectDate (Maybe Date)
    | OverDate (Maybe Date)
    | CancelDates
    | ClearDates


{-| The Config that is passed to [`showCalendar`](#showCalendar) to control html classes and valid dates.

  - **rangeClass**: The html class to apply to the selected date range.

  - **rangeHoverClass**: The html class to apply to the selected date range on hover.

  - **selectedClass**: The html class to apply to a single selected date.

  - **disabledClass**: The html class to apply to a disabled/invalid dates.

  - **validClass**: The html class to apply to a valid/selectable dates.

  - **dayClass**: The html class to apply to all dates.

  - **calendarClass**: The html class to apply to the calendar container.

  - **titleClass**: THe html class to apply to the calendar title.

  - **weekdayFormat**: The format of the Weekday labels (Mon, Tue etc.). options:
      - **"d"**: M, T, W, T, F, S, S
      - **"dd"**: M, Tu, W, Th, F, Sa, Su
      - **"ddd"**: Mon, Tue, Wed, Thu, Fri, Sat, Sun
      - **"D"**: Monday, Tuesday, Wednesday, Thursday, Friday, Saturday

  - **weekdayFormatter**: Custom implementation of `weekdayFormat`, needed to support non-english languages.

  - **titleFormatter**: Format title based on year and month.

  - **validDate**: A function that takes a single date, and the current date, and returns true if that date is available to select,
    or false if not.

-}
type alias Config =
    { rangeClass : String
    , rangeHoverClass : String
    , selectedClass : String
    , disabledClass : String
    , validClass : String
    , dayClass : String
    , calendarClass : String
    , titleClass : String
    , weekdayFormat : String
    , weekdayFormatter : String -> Day -> String
    , titleFormatter : Int -> Month -> String
    , validDate : Maybe Date -> Maybe Date -> Bool
    }


{-| The function called to initialise the Calendar. Returns the initial model.
Pass it a [`Selection`](#Selection) type to determine whether it will be used
to select a single date, or a range of dates.

    init : ( Model, Cmd Msg )
    init =
        { calendar = DatePicker.initCalendar DatePicker.Single } ! []

-}
initCalendar : Selection -> DatePicker
initCalendar selection =
    let
        selectDate =
            case selection of
                Range ->
                    From

                Single ->
                    Only
    in
        DatePicker <|
            { currentDate = Nothing
            , month = ( 2018, Jan, [] )
            , nextMonth = ( 2018, Feb, [] )
            , open = False
            , selectDate = selectDate
            , from = Nothing
            , to = Nothing
            , single = Nothing
            , overDate = Nothing
            }


{-| The default config options that will be applied if not overwritten with a config argument to [`showCalendar`](#showCalendar)

    defaultConfig : Config
    defaultConfig =
        { rangeClass = "bg-dark-blue white"
        , rangeHoverClass = "bg-dark-blue moon-gray"
        , selectedClass = "bg-dark-blue white"
        , dayClass = "pa1"
        , disabledClass = "moon-gray"
        , validClass = "pointer"
        , calendarClass = "pa3 dib gray"
        , titleClass = "tc"
        , weekdayFormat = "ddd"
        , weekdayFormatter = defaultWeekdayFormatter
        , titleFormatter = defaultTitleFormatter
        , validDate = validDate
        }

    validDate : Maybe Date -> Maybe Date -> Bool
    validDate date currentDate =
        DateCore.greaterOrEqual date currentDate

Note: We use [`tachyons`](https://github.com/tachyons-css/tachyons) in our default classes, but you don't have to.
We don't provide any css, so it's up to you to define these styles, or include [`tachyons from the cdn`](https://cdnjs.com/libraries/tachyons),
or override them by passing a [`custom config`](#Config) to [`showCalendar`](#showCalendar)

-}
defaultConfig : Config
defaultConfig =
    { rangeClass = "bg-dark-blue white"
    , rangeHoverClass = "bg-dark-blue moon-gray"
    , selectedClass = "bg-dark-blue white"
    , dayClass = "pa1"
    , disabledClass = "moon-gray"
    , validClass = "pointer"
    , calendarClass = "pa3 dib gray"
    , titleClass = "tc"
    , weekdayFormat = "ddd"
    , weekdayFormatter = defaultWeekdayFormatter
    , titleFormatter = defaultTitleFormatter
    , validDate = validDate
    }


defaultTitleFormatter : Int -> Month -> String
defaultTitleFormatter year month =
    toString month ++ " " ++ toString year


defaultWeekdayFormatter : String -> Day -> String
defaultWeekdayFormatter format day =
    case format of
        "d" ->
            String.left 1 (toString day)

        "ddd" ->
            toString day

        "D" ->
            case day of
                Mon ->
                    "Monday"

                Tue ->
                    "Tuesday"

                Wed ->
                    "Wednesday"

                Thu ->
                    "Thursday"

                Fri ->
                    "Friday"

                Sat ->
                    "Saturday"

                Sun ->
                    "Sunday"

        _ ->
            case day of
                Mon ->
                    "M"

                Tue ->
                    "Tu"

                Wed ->
                    "W"

                Thu ->
                    "Th"

                Fri ->
                    "F"

                Sat ->
                    "Sa"

                Sun ->
                    "Su"


validDate : Maybe Date -> Maybe Date -> Bool
validDate date currentDate =
    DateCore.greaterOrEqual date currentDate


isDateSelected : Maybe Date -> Maybe Date -> Bool
isDateSelected date1 date2 =
    case date1 of
        Just d1 ->
            case date2 of
                Just d2 ->
                    DateCore.equal d1 d2

                Nothing ->
                    False

        Nothing ->
            False


showDate : DatePicker -> Config -> Maybe Date -> Html Msg
showDate (DatePicker { currentDate, from, to, single, overDate }) config date =
    let
        fromSelected =
            isDateSelected date from

        toSelected =
            isDateSelected date to

        singleSelected =
            isDateSelected date single

        selected =
            fromSelected || toSelected || singleSelected

        insideRange =
            DateCore.inRange date from to

        insideRangeOver =
            case ( from, to ) of
                ( Just _, Nothing ) ->
                    DateCore.inRange date from overDate

                ( _, _ ) ->
                    False
    in
        case date of
            Just _ ->
                if config.validDate date currentDate then
                    td
                        [ class config.dayClass
                        , classList [ ( config.validClass, not selected ), ( config.selectedClass, selected ), ( config.rangeClass, insideRange ), ( config.rangeHoverClass, insideRangeOver ) ]
                        , onClick (SelectDate date)
                        , onMouseOver (OverDate date)
                        , tabindex 0
                        , attribute "role" "option"
                        , attribute "aria-selected" <| toString selected
                        , onEnter (SelectDate date)
                        ]
                        [ text <| DateCore.getFormattedDate date ]
                else
                    td [ class (config.dayClass ++ " " ++ config.disabledClass) ] [ text <| DateCore.getFormattedDate date ]

            Nothing ->
                td [ class config.dayClass ] [ text <| DateCore.getFormattedDate date ]


getDates : Year -> Month -> List (Maybe Date)
getDates year month =
    let
        daysInMonth =
            DateCore.daysInMonth year month

        datesOfMonth =
            List.map (DateCore.toDate year (DateCore.monthToInt month)) <| List.range 1 daysInMonth

        firstDay =
            List.head datesOfMonth

        lastDay =
            List.head <| List.reverse datesOfMonth

        prepend =
            DateCore.nothingToMonday (firstDay)

        append =
            DateCore.nothingToSunday (lastDay)
    in
        prepend ++ datesOfMonth ++ append


showMonth : DatePicker -> Config -> List (Maybe Date) -> List (Html Msg)
showMonth (DatePicker model) config month =
    List.map (showWeek (DatePicker model) config) (DateCore.groupByWeek month)


showWeek : DatePicker -> Config -> List (Maybe Date) -> Html Msg
showWeek (DatePicker model) config week =
    tr [] (List.map (showDate (DatePicker model) config) week)


{-| The function called to show the calendar. Pass it a config record to customise the DatePicker:

    view : Model -> Html Msg
    view =
        model
            div
            []
            [ (DatePicker.showCalendar model.calendar (DatePicker.getMonth model.calendar) config) |> Html.map DatePickerMsg ]

    config : DatePicker.Config
    config =
        let
            config =
                DatePicker.defaultConfig
        in
            { config
                | weekdayFormat = "ddd"
                , validDate = validDate
            }

    type Msg
        = DatePickerMsg DatePicker.Msg

-}
showCalendar : DatePicker -> MonthData -> Config -> Html Msg
showCalendar (DatePicker model) monthData config =
    let
        ( year, month, dates ) =
            monthData

        title =
            config.titleFormatter year month

        multiselectable =
            model.selectDate /= Only |> toString
    in
        div [ class config.calendarClass ]
            [ h1 [ class config.titleClass, id "title" ] [ text title ]
            , table []
                [ thead []
                    [ tr []
                        (renderWeekdays config)
                    ]
                , tbody
                    [ attribute "role" "listbox"
                    , attribute "aria-multiselectable" multiselectable
                    ]
                    (showMonth (DatePicker model) config dates)
                ]
            ]


renderWeekdays : Config -> List (Html Msg)
renderWeekdays config =
    let
        days =
            [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]

        format =
            config.weekdayFormatter config.weekdayFormat
    in
        List.map (\day -> td [ class config.dayClass ] [ format day |> text ]) days


{-| The DatePicker update function
-}
update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
    case msg of
        ReceiveDate date ->
            let
                currentDate =
                    Just date

                ( year1, month1 ) =
                    DateCore.getYearAndMonth currentDate

                currentMonth =
                    getDates year1 month1

                ( year2, month2 ) =
                    getYearAndMonthNext year1 month1

                nextMonth =
                    getDates year2 month2
            in
                DatePicker ({ model | month = ( year1, month1, currentMonth ), nextMonth = ( year2, month2, nextMonth ), currentDate = currentDate })

        ToggleCalendar ->
            DatePicker ({ model | open = not model.open })

        PreviousMonth ->
            let
                ( year, month, _ ) =
                    model.month

                ( prevYear, prevMonth ) =
                    getYearAndMonthPrevious year month

                prevData =
                    getDates prevYear prevMonth
            in
                DatePicker ({ model | month = ( prevYear, prevMonth, prevData ), nextMonth = model.month })

        NextMonth ->
            let
                ( year, month, _ ) =
                    model.nextMonth

                ( nextYear, nextMonth ) =
                    getYearAndMonthNext year month

                nextData =
                    getDates nextYear nextMonth
            in
                DatePicker ({ model | month = model.nextMonth, nextMonth = ( nextYear, nextMonth, nextData ) })

        SelectDate date ->
            case date of
                Just d ->
                    let
                        update =
                            case model.selectDate of
                                From ->
                                    if DateCore.greaterOrEqual date model.to then
                                        DatePicker ({ model | from = date, to = Nothing, selectDate = To })
                                    else
                                        DatePicker ({ model | from = date, selectDate = To })

                                To ->
                                    if DateCore.lowerOrEqual date model.from then
                                        DatePicker ({ model | from = date, to = Nothing, selectDate = To })
                                    else
                                        DatePicker ({ model | to = date, selectDate = From })

                                Only ->
                                    DatePicker ({ model | single = date, selectDate = Only })
                    in
                        update

                Nothing ->
                    (DatePicker model)

        OverDate date ->
            DatePicker ({ model | overDate = date })

        CancelDates ->
            let
                selection =
                    case model.selectDate of
                        Only ->
                            Only

                        _ ->
                            From
            in
                DatePicker ({ model | from = Nothing, to = Nothing, single = Nothing, open = False, selectDate = selection })

        ClearDates ->
            let
                selection =
                    case model.selectDate of
                        Only ->
                            Only

                        _ ->
                            From
            in
                DatePicker ({ model | from = Nothing, to = Nothing, single = Nothing, selectDate = selection, overDate = Nothing })


{-| Get the `from` date in a selected range
-}
getFrom : DatePicker -> Maybe Date
getFrom (DatePicker model) =
    model.from


{-| Get the `to` date in a selected range
-}
getTo : DatePicker -> Maybe Date
getTo (DatePicker model) =
    model.to


{-| Get the single selected date
-}
getSelectedDate : DatePicker -> Maybe Date
getSelectedDate (DatePicker model) =
    model.single


{-| Get the current month
-}
getMonth : DatePicker -> MonthData
getMonth (DatePicker model) =
    model.month


{-| Get the next month
-}
getNextMonth : DatePicker -> MonthData
getNextMonth (DatePicker model) =
    model.nextMonth


{-| Returns whether the calendar is open or not
-}
isOpen : DatePicker -> Bool
isOpen (DatePicker model) =
    model.open


{-| Clears all selected dates
-}
clearDates : DatePicker -> DatePicker
clearDates =
    update ClearDates


{-| Closes or opens calendar
-}
toggleCalendar : DatePicker -> DatePicker
toggleCalendar =
    update ToggleCalendar


{-| Clears all selected dates and closes calendar
-}
cancelDates : DatePicker -> DatePicker
cancelDates =
    update CancelDates


{-| Sets current month to previous month, and next month to current month
-}
previousMonth : DatePicker -> DatePicker
previousMonth =
    update PreviousMonth


{-| Sets current month to next month, and next month to next month + 1
-}
nextMonth : DatePicker -> DatePicker
nextMonth =
    update NextMonth


{-| Gets current date to initialise the calendar
-}
receiveDate : Cmd Msg
receiveDate =
    Task.perform ReceiveDate Date.now


{-| Manually set the selected date
-}
setDate : Date -> DatePicker -> DatePicker
setDate date =
    update (SelectDate (Just date))
