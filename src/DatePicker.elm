module DatePicker exposing (..)

import Html.Events exposing (onClick, onInput, onMouseOver)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Json exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time
import Process
import Task
import DateCore exposing (..)
import Date exposing (..)
import Task exposing (perform)


type alias MonthData =
    ( Year, Month, List (Maybe Date) )


type FromTo
    = From
    | To


type alias Model =
    { currentDate : Maybe Date
    , month : MonthData
    , nextMonth : MonthData
    , open : Bool
    , selectDate : FromTo
    , from : Maybe Date
    , to : Maybe Date
    , overDate : Maybe Date
    , config : Config
    }


type Msg
    = ReceiveDate Date
    | ToggleCalendar
    | PreviousMonth
    | NextMonth
    | SelectDate (Maybe Date)
    | OverDate (Maybe Date)
    | CancelDates
    | ApplyDates
    | ClearDates


type alias Config =
    { rangeClass : String
    , rangeHoverClass : String
    , selectedClass : String
    , disabledClass : String
    , validClass : String
    , dayClass : String
    }


initCalendar : Model
initCalendar =
    { currentDate = Nothing
    , month = ( 2018, Jan, [] )
    , nextMonth = ( 2018, Jan, [] )
    , open = False
    , selectDate = From
    , from = Nothing
    , to = Nothing
    , overDate = Nothing
    , config = defaultConfig
    }


defaultConfig : Config
defaultConfig =
    { rangeClass = "bg-dark-pink white"
    , rangeHoverClass = "bg-dark-pink moon-gray"
    , selectedClass = "bg-moon-gray"
    , dayClass = "pa1"
    , disabledClass = "moon-gray"
    , validClass = "pointer"
    }


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


showDate : Model -> Maybe Date -> Maybe Date -> Maybe Date -> Maybe Date -> Maybe Date -> Html Msg
showDate model currentDate from to overDate date =
    let
        next2days =
            (DateCore.getNextDay >> DateCore.getNextDay) currentDate

        next18monthDate =
            get18monthDate currentDate

        validDate =
            (DateCore.greater date next2days) && (DateCore.lowerOrEqual date next18monthDate)

        fromSelected =
            isDateSelected date from

        toSelected =
            isDateSelected date to

        selected =
            fromSelected || toSelected

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
                if validDate then
                    td [ class (model.config.dayClass ++ " " ++ model.config.validClass), classList [ ( model.config.selectedClass, selected ), ( model.config.rangeClass, insideRange ), ( model.config.rangeHoverClass, insideRangeOver ) ], onClick (SelectDate date), onMouseOver (OverDate date) ] [ text <| DateCore.getFormattedDate date ]
                else
                    td [ class (model.config.dayClass ++ " " ++ model.config.disabledClass) ] [ text <| DateCore.getFormattedDate date ]

            Nothing ->
                td [ class model.config.dayClass ] [ text <| DateCore.getFormattedDate date ]



-- getNextMonth : Maybe Date
-- Create a Maybe Date from string ex: 2018-01-01 = Just <2018...>
-- Crate a string from Ints year month day: 2018 1 1 = "2018-1-1"


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


showMonth : Model -> Maybe Date -> List (Maybe Date) -> Maybe Date -> Maybe Date -> Maybe Date -> List (Html Msg)
showMonth model currentDate month from to overDate =
    List.map (showWeek model currentDate from to overDate) (DateCore.groupByWeek month)


showWeek : Model -> Maybe Date -> Maybe Date -> Maybe Date -> Maybe Date -> List (Maybe Date) -> Html Msg
showWeek model currentDate from to overDate week =
    tr [] (List.map (showDate model currentDate from to overDate) week)


showCalendar : Model -> Maybe Date -> MonthData -> Maybe Date -> Maybe Date -> Maybe Date -> Html Msg
showCalendar model currentDate ( year, month, dates ) from to overDate =
    div [ class "pa3 dib gray" ]
        [ h1 [] [ text (toString year ++ " " ++ toString month) ]
        , table []
            [ thead []
                [ tr []
                    [ td [ class "pa1" ] [ text "M" ]
                    , td [ class "pa1" ] [ text "Tu" ]
                    , td [ class "pa1" ] [ text "W" ]
                    , td [ class "pa1" ] [ text "Th" ]
                    , td [ class "pa1" ] [ text "F" ]
                    , td [ class "pa1" ] [ text "Sa" ]
                    , td [ class "pa1" ] [ text "Su" ]
                    ]
                ]
            , tbody [] (showMonth model currentDate dates from to overDate)
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                ( { model | month = ( year1, month1, currentMonth ), nextMonth = ( year2, month2, nextMonth ), currentDate = currentDate }, Cmd.none )

        ToggleCalendar ->
            ( { model | open = not model.open }, Cmd.none )

        PreviousMonth ->
            let
                ( year, month, _ ) =
                    model.month

                ( prevYear, prevMonth ) =
                    getYearAndMonthPrevious year month

                prevData =
                    getDates prevYear prevMonth
            in
                ( { model | month = ( prevYear, prevMonth, prevData ), nextMonth = model.month }, Cmd.none )

        NextMonth ->
            let
                ( year, month, _ ) =
                    model.nextMonth

                ( nextYear, nextMonth ) =
                    getYearAndMonthNext year month

                nextData =
                    getDates nextYear nextMonth

                update =
                    { model | month = model.nextMonth, nextMonth = ( nextYear, nextMonth, nextData ) }
            in
                ( update, Cmd.none )

        SelectDate date ->
            case date of
                Just d ->
                    let
                        update =
                            case model.selectDate of
                                From ->
                                    if DateCore.greaterOrEqual date model.to then
                                        { model | from = date, to = Nothing, selectDate = To }
                                    else
                                        { model | from = date, selectDate = To }

                                To ->
                                    if DateCore.lowerOrEqual date model.from then
                                        { model | from = date, to = Nothing, selectDate = To }
                                    else
                                        { model | to = date, selectDate = From }
                    in
                        ( update, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OverDate date ->
            ( { model | overDate = date }, Cmd.none )

        CancelDates ->
            ( { model | from = Nothing, to = Nothing, open = False, selectDate = From }, Cmd.none )

        ApplyDates ->
            ( { model | open = False }, Cmd.none )

        ClearDates ->
            ( { model | from = Nothing, to = Nothing, selectDate = From }, Cmd.none )
