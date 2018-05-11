module Dropdown exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import DatePicker
import Date exposing (Date)
import Task


type alias Model =
    { calendar : DatePicker.DatePicker, selectedDate : Maybe Date }


type Msg
    = DatePickerMsg DatePicker.Msg
    | PreviousMonth
    | NextMonth
    | Toggle
    | ClearDate


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { calendar = DatePicker.initCalendar DatePicker.Single, selectedDate = Nothing }
    , Cmd.map DatePickerMsg DatePicker.receiveDate
    )


view : Model -> Html Msg
view model =
    let
        title =
            case model.selectedDate of
                Just date ->
                    singleDate date

                Nothing ->
                    noDate "Select Date"
    in
        div [ class "dib w-50 w-third-l relative b--moon-gray" ]
            [ h2 [ class "helvetica" ] [ text "Dropdown DatePicker" ]
            , title
            , div
                [ classList [ ( "dn", not (DatePicker.isOpen model.calendar) ), ( "dib", DatePicker.isOpen model.calendar ) ], class "absolute top-3 left-0 bg-white w-100 ba b--moon-gray pt2 pb3 z-3" ]
                [ span [ class "pointer pa2 gray bn pointer", onClick PreviousMonth ] [ text "<" ]
                , DatePicker.showCalendar model.calendar (DatePicker.getMonth model.calendar) (config) |> Html.map DatePickerMsg
                , span [ class "pointer pa2 gray bn pointer", onClick NextMonth ] [ text ">" ]
                ]
            ]


singleDate : Date -> Html Msg
singleDate date =
    div [ onClick Toggle, class "pointer bg-white db mb3 mv1-l w-100 helvetica" ]
        [ div
            [ class "bg-light-gray ba b--moon-gray br-pill flex justify-between items-center overflow-hidden ml1-l" ]
            [ p [ class "pointer ph2 ml3 f6" ]
                [ text <| (formatDate date) ]
            , p [ onClick ClearDate, class "bg-light-gray ph2 pointer br-pill mr3 f6" ] [ text "X" ]
            ]
        ]


noDate : String -> Html Msg
noDate whenText =
    div [ onClick Toggle, class "ba b--moon-gray pointer helvetica" ]
        [ p [ class "dib w-70 b--moon-gray br1 ba bn-l f5 lh6 gray bg-white br" ] [ text whenText ]
        , span [ class "gray dib tr w-20 mr3" ] [ text "▼" ]
        ]


formatDate : Date -> String
formatDate date =
    (toString (Date.day date)) ++ " " ++ (toString (Date.month date)) ++ " " ++ (toString (Date.year date))


config : DatePicker.Config
config =
    let
        config =
            DatePicker.defaultConfig
    in
        { config
            | selectedClass = "bg-moon-gray white selected"
            , weekdayFormat = "ddd"
            , validDate = validDate
        }


validDate : Maybe Date -> Maybe Date -> Bool
validDate date currentDate =
    case ( date, currentDate ) of
        ( _, Nothing ) ->
            True

        ( Just date1, Just date2 ) ->
            (Date.toTime date1) > (Date.toTime date2)

        ( Nothing, Just _ ) ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatePickerMsg datePickerMsg ->
            let
                newCalendar =
                    DatePicker.update datePickerMsg model.calendar
            in
                { model | calendar = newCalendar, selectedDate = DatePicker.getSelectedDate newCalendar } ! []

        PreviousMonth ->
            { model | calendar = DatePicker.previousMonth model.calendar } ! []

        NextMonth ->
            { model | calendar = DatePicker.nextMonth model.calendar } ! []

        Toggle ->
            { model | calendar = DatePicker.toggleCalendar model.calendar } ! []

        ClearDate ->
            { model | calendar = DatePicker.clearDates model.calendar, selectedDate = Nothing } ! []
