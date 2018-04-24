module Example exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import DatePicker
import Date exposing (Date)
import DateCore exposing (..)
import Task


type alias Model =
    { calendar : DatePicker.Model }


type Msg
    = DatePickerMsg DatePicker.Msg


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
    ( { calendar = DatePicker.initCalendar }
    , Cmd.map DatePickerMsg (Task.perform DatePicker.ReceiveDate Date.now)
    )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ (DatePicker.showCalendar model.calendar viewConfig) |> Html.map DatePickerMsg ]
        ]


viewConfig : DatePicker.Config
viewConfig =
    let
        config =
            DatePicker.defaultViewConfig
    in
        { config
            | rangeClass = "bg-dark-blue white"
            , rangeHoverClass = "bg-dark-blue moon-gray"
            , selectedClass = "bg-moon-gray"
            , weekdayFormat = "ddd"
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatePickerMsg datePickerMsg ->
            datePickerUpdate datePickerMsg model


datePickerUpdate : DatePicker.Msg -> Model -> ( Model, Cmd Msg )
datePickerUpdate datePickerMsg model =
    let
        ( dateModel, dateCmd ) =
            DatePicker.update datePickerMsg model.calendar
    in
        { model | calendar = dateModel } ! [ Cmd.map DatePickerMsg dateCmd ]
