module TwoMonth exposing (main)

import Browser
import Date exposing (Date)
import DatePicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { calendar : DatePicker.DatePicker }


type Msg
    = DatePickerMsg DatePicker.Msg
    | PreviousMonth
    | NextMonth


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { calendar = DatePicker.initCalendar DatePicker.Single }
    , Cmd.map DatePickerMsg DatePicker.receiveDate
    )


view : Model -> Html Msg
view model =
    let
        ( year, month ) =
            DatePicker.getMonth model.calendar

        nextMonth =
            Date.fromCalendarDate year month 1
                |> Date.add Date.Months 1
    in
    div []
        [ h2 [ class "helvetica" ] [ text "Two Month DatePicker" ]
        , button [ id "previous-month", class "bn gray pointer", onClick PreviousMonth ] [ text "<" ]
        , DatePicker.showCalendar model.calendar config
            |> Html.map DatePickerMsg
        , DatePicker.showCalendarForMonth ( Date.year nextMonth, Date.month nextMonth ) model.calendar config
            |> Html.map DatePickerMsg
        , button [ id "next-month", class "bn gray pointer", onClick NextMonth ] [ text ">" ]
        ]


config : DatePicker.Config
config =
    let
        config_ =
            DatePicker.defaultConfig
    in
    { config_
        | rangeClass = "bg-dark-red white"
        , rangeHoverClass = "bg-dark-red moon-gray"
        , selectedClass = "bg-dark-red white selected"
        , weekdayFormat = "ddd"
        , validDate = validDate
    }


validDate : Maybe Date -> Maybe Date -> Bool
validDate date currentDate =
    case ( date, currentDate ) of
        ( _, Nothing ) ->
            True

        ( Just date1, Just date2 ) ->
            Date.toRataDie date1 > Date.toRataDie date2

        ( Nothing, Just _ ) ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatePickerMsg datePickerMsg ->
            ( { model | calendar = DatePicker.update datePickerMsg model.calendar }
            , Cmd.none
            )

        PreviousMonth ->
            ( { model | calendar = DatePicker.previousMonth model.calendar }
            , Cmd.none
            )

        NextMonth ->
            ( { model | calendar = DatePicker.nextMonth model.calendar }
            , Cmd.none
            )
