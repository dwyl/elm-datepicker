## Elm DatePicker

A customisble Datepicker written in Elm.

### Usage

Initialise the DatePicker by calling `DatePicker.initCalendar`. Provide it with a single argument: `Single` if you just want to be able to select a single date, or `Range` if you want to select a range.

It's a good idea here to set up a Msg type that you can use to communicate with the DatePicker module.
``` Elm
type alias Model =
    { calendar : DatePicker.DatePicker }


type Msg
    = DatePickerMsg DatePicker.Msg


init : ( Model, Cmd Msg )
init =
    ( { calendar = DatePicker.initCalendar DatePicker.Single }
    , Cmd.map DatePickerMsg (Task.perform DatePicker.ReceiveDate Date.now)
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...
        DatePickerMsg datePickerMsg ->
            { model | calendar = DatePicker.update datePickerMsg model.calendar } ! [  ]
```

To display the DatePicker, call `DatePicker.showCalendar`. This takes the initialised datepicker as its first argument, the month you want to display as the second, and a configuration record as the third.

Don't forget, the Html will return a `DatePicker.Msg`, so you have to map it to the `DatePickerMsg Msg` we set up above, using `Html.map DatePickerMsg`.
``` Elm
view : Model -> Html Msg
view model =
  div []
    [ DatePicker.showCalendar model.calendar (DatePicker.getMonth model.calendar) config
        |> Html.map DatePickerMsg
    ]


config : DatePicker.Config
config =
    let
        config =
            DatePicker.defaultConfig
    in
        { config
            | rangeClass = "bg-dark-blue white"
            , rangeHoverClass = "bg-dark-blue moon-gray"
            , selectedClass = "bg-gray white"
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
```

## Examples
To run the examples:
```sh
  elm make examples/Main.elm --output=examples/examples.js
  elm-reactor
```

Then visit `localhost:8000` and open `index.html`
