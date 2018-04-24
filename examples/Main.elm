module Example exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import DatePicker
import Date exposing (Date)
import DateCore exposing (..)
import Task


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ (DatePicker.showCalendar model.calendar model.calendar.currentDate model.calendar.month model.calendar.from model.calendar.to model.calendar.overDate) |> Html.map DatePickerMsg ]
        ]


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


init : ( Model, Cmd Msg )
init =
    ( { calendar = DatePicker.initCalendar }
    , Cmd.map DatePickerMsg (Task.perform DatePicker.ReceiveDate Date.now)
    )


type alias Model =
    { calendar : DatePicker.Model }


type Msg
    = DatePickerMsg DatePicker.Msg
