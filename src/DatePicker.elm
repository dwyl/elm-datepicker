module DatePicker exposing
    ( DatePicker, Msg, Config, defaultConfig, initCalendar, showCalendar, showCalendarForMonth, update, Selection(..), receiveDate
    , getFrom, getTo, getMonth, isOpen, getSelectedDate
    , clearDates, toggleCalendar, cancelDates, previousMonth, nextMonth, setDate, setMonth
    )

{-| A customisable DatePicker that easily allows you to select a range of dates

@docs DatePicker, Msg, Config, defaultConfig, initCalendar, showCalendar, showCalendarForMonth, update, Selection, receiveDate


# Getter Functions

These functions allow you to access data from the DatePicker model.

@docs getFrom, getTo, getMonth, isOpen, getSelectedDate


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

@docs clearDates, toggleCalendar, cancelDates, previousMonth, nextMonth, setDate, setMonth

-}

import Date exposing (Date)
import DateCore exposing (Year)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseOver)
import Json.Decode
import Task
import Time exposing (Month(..), Weekday(..))


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
    , month : ( Year, Month, List (Maybe Date) )
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
    , weekdayFormatter : String -> Weekday -> String
    , titleFormatter : Year -> Month -> String
    , validDate : Maybe Date -> Maybe Date -> Bool
    }


{-| The function called to initialise the Calendar. Returns the initial model.
Pass it a [`Selection`](#Selection) type to determine whether it will be used
to select a single date, or a range of dates.

    init : ( Model, Cmd Msg )
    init =
        ( { calendar = DatePicker.initCalendar DatePicker.Single }
        , Cmd.none
        )

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
    DatePicker
        { currentDate = Nothing
        , month = ( 2018, Jan, [] )
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


defaultTitleFormatter : Year -> Month -> String
defaultTitleFormatter year month =
    DateCore.monthToString month ++ " " ++ String.fromInt year


defaultWeekdayFormatter : String -> Weekday -> String
defaultWeekdayFormatter format day =
    let
        dateOnGivenWeekday =
            Date.fromWeekDate 2018 2 day
    in
    case format of
        "d" ->
            dateOnGivenWeekday
                |> Date.format "EEE"
                |> String.left 1

        "ddd" ->
            dateOnGivenWeekday
                |> Date.format "EEE"

        "D" ->
            dateOnGivenWeekday
                |> Date.format "EEEE"

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


showDate : DatePicker -> Config -> Maybe Date -> Html Msg
showDate (DatePicker { currentDate, from, to, single, overDate }) config date =
    let
        selected =
            List.any (DateCore.equal date) [ from, to, single ]

        insideRange =
            DateCore.inRange date from to

        insideRangeOver =
            case ( from, to ) of
                ( Just _, Nothing ) ->
                    DateCore.inRange date from overDate

                ( _, _ ) ->
                    False

        attributes =
            if config.validDate date currentDate then
                [ class config.dayClass
                , classList
                    [ ( config.validClass, not selected )
                    , ( config.selectedClass, selected )
                    , ( config.rangeClass, insideRange )
                    , ( config.rangeHoverClass, insideRangeOver )
                    ]
                , onClick (SelectDate date)
                , onMouseOver (OverDate date)
                , tabindex 0
                , attribute "role" "option"
                , attribute "aria-selected" <| boolToString selected
                , onEnter (SelectDate date)
                ]

            else
                case date of
                    Just _ ->
                        [ class (config.dayClass ++ " " ++ config.disabledClass) ]

                    Nothing ->
                        [ class config.dayClass ]
    in
    td attributes [ text <| DateCore.getFormattedDate date ]


getDates : Year -> Month -> List (Maybe Date)
getDates year month =
    let
        datesOfMonth =
            DateCore.datesOfMonth year month
    in
    List.concat
        [ List.head datesOfMonth
            |> DateCore.nothingToMonday
        , List.map Just datesOfMonth
        , List.reverse datesOfMonth
            |> List.head
            |> DateCore.nothingToSunday
        ]


showMonth : DatePicker -> Config -> List (Maybe Date) -> List (Html Msg)
showMonth (DatePicker model) config month =
    List.map (showWeek (DatePicker model) config) (DateCore.groupByWeek month)


showWeek : DatePicker -> Config -> List (Maybe Date) -> Html Msg
showWeek (DatePicker model) config week =
    tr [] (List.map (showDate (DatePicker model) config) week)


{-| The function called to show the calendar. Pass it a config record to customise the DatePicker:

    view : Model -> Html Msg
    view model =
        div []
            [ DatePicker.showCalendar model.calendar config
                |> Html.map DatePickerMsg
            ]

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
showCalendar : DatePicker -> Config -> Html Msg
showCalendar (DatePicker model) =
    showCalendar_ model.month (DatePicker model)


{-| Show any month from the calendar
-}
showCalendarForMonth : ( Year, Month ) -> DatePicker -> Config -> Html Msg
showCalendarForMonth ( year, month ) =
    let
        currentMonth =
            getDates year month
    in
    showCalendar_ ( year, month, currentMonth )


showCalendar_ : ( Year, Month, List (Maybe Date) ) -> DatePicker -> Config -> Html Msg
showCalendar_ monthData (DatePicker model) config =
    let
        ( year, month, dates ) =
            monthData

        title =
            config.titleFormatter year month

        multiselectable =
            model.selectDate /= Only |> boolToString
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
            DatePicker { model | currentDate = Just date }
                |> setMonth ( Date.year date, Date.month date )

        ToggleCalendar ->
            DatePicker { model | open = not model.open }

        PreviousMonth ->
            let
                ( year, month ) =
                    getMonth (DatePicker model)

                ( yearPrev, monthPrev ) =
                    DateCore.getYearAndMonthPrevious year month
            in
            DatePicker model
                |> setMonth ( yearPrev, monthPrev )

        NextMonth ->
            let
                ( year, month ) =
                    getMonth (DatePicker model)

                ( yearNext, monthNext ) =
                    DateCore.getYearAndMonthNext year month
            in
            DatePicker model
                |> setMonth ( yearNext, monthNext )

        SelectDate date ->
            case date of
                Just d ->
                    let
                        update_ =
                            case model.selectDate of
                                From ->
                                    if DateCore.greaterOrEqual date model.to then
                                        DatePicker { model | from = date, to = Nothing, selectDate = To }

                                    else
                                        DatePicker { model | from = date, selectDate = To }

                                To ->
                                    if DateCore.lowerOrEqual date model.from then
                                        DatePicker { model | from = date, to = Nothing, selectDate = To }

                                    else
                                        DatePicker { model | to = date, selectDate = From }

                                Only ->
                                    DatePicker { model | single = date, selectDate = Only }
                    in
                    update_

                Nothing ->
                    DatePicker model

        OverDate date ->
            DatePicker { model | overDate = date }

        CancelDates ->
            let
                selection =
                    case model.selectDate of
                        Only ->
                            Only

                        _ ->
                            From
            in
            DatePicker { model | from = Nothing, to = Nothing, single = Nothing, open = False, selectDate = selection }

        ClearDates ->
            let
                selection =
                    case model.selectDate of
                        Only ->
                            Only

                        _ ->
                            From
            in
            DatePicker { model | from = Nothing, to = Nothing, single = Nothing, selectDate = selection, overDate = Nothing }


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
getMonth : DatePicker -> ( Year, Month )
getMonth (DatePicker model) =
    let
        ( year, month, _ ) =
            model.month
    in
    ( year, month )


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
    Task.perform ReceiveDate Date.today


{-| Manually set the selected date
-}
setDate : Date -> DatePicker -> DatePicker
setDate date =
    update (SelectDate (Just date))
        >> setMonth ( Date.year date, Date.month date )


{-| Manually set the selected month
-}
setMonth : ( Year, Month ) -> DatePicker -> DatePicker
setMonth ( year, month ) (DatePicker model) =
    DatePicker { model | month = ( year, month, getDates year month ) }


{-| When the enter key is released, send the `msg`. Otherwise, do nothing.
-}
onEnter : msg -> Html.Attribute msg
onEnter onEnterAction =
    Html.Events.on "keyup" <|
        Json.Decode.andThen
            (\keyCode ->
                if keyCode == 13 then
                    Json.Decode.succeed onEnterAction

                else
                    Json.Decode.fail (String.fromInt keyCode)
            )
            Html.Events.keyCode


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"
