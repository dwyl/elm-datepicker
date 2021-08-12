module Simple exposing (main)

import Browser
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
    div []
        [ h2 [ class "helvetica m0" ] [ text "Simple DatePicker" ]
        , button [ id "previous-month", class "bn pointer gray", onClick PreviousMonth, attribute "aria-label" "previous month"] [ text "<" ]
        , DatePicker.showCalendar model.calendar (DatePicker.getMonth model.calendar) DatePicker.defaultConfig
            |> Html.map DatePickerMsg
        , button [ id "next-month", class "bn pointer gray", onClick NextMonth, attribute "aria-label" "next month" ] [ text ">" ]
        ]


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
