## Elm DatePicker

A customisable Datepicker written in Elm.

[![Build Status](https://travis-ci.org/dwyl/elm-datepicker.svg?branch=master)](https://travis-ci.org/dwyl/elm-datepicker)

### Usage

Initialise the DatePicker by calling `DatePicker.initCalendar`. Provide it with a single argument: `Single` if you just want to be able to select a single date, or `Range` if you want to select a range.

It's a good idea here to set up a Msg type that you can use to forward DatePicker messages the DatePicker update function.

```Elm
type alias Model =
    { calendar : DatePicker.DatePicker }


type Msg
    = DatePickerMsg DatePicker.Msg


init : ( Model, Cmd Msg )
init =
    ( { calendar = DatePicker.initCalendar DatePicker.Single }
    , Cmd.map DatePickerMsg DatePicker.receiveDate
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...
        DatePickerMsg datePickerMsg ->
            { model | calendar = DatePicker.update datePickerMsg model.calendar } ! []
```

To display the DatePicker, call `DatePicker.showCalendar`. This takes the initialised datepicker as its first argument, the month you want to display as the second, and a configuration record as the third.

Don't forget, the Html will return a `DatePicker.Msg`, so you have to map it to the `DatePickerMsg Msg` we set up above, using `Html.map DatePickerMsg`.

```Elm
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

## Tests

First, ensure that you have the dependencies installed:

```sh
elm-package install -y
npm install
```

_Then_ run the elm tests on your `localhost`,
using the following command in your Terminal:

```sh
elm-test --verbose
```

And run the cypress tests with:

```sh
./cypress-tests.sh
```

If you want to see the tests running in the [cypress test runner](https://docs.cypress.io/guides/core-concepts/test-runner.html#), make sure elm-reactor is running, then run the cypress tests:

```sh
./node_modules/.bin/cypress open
```

[![HitCount](http://hits.dwyl.io/dwyl/elm-datepicker.svg)](http://hits.dwyl.io/dwyl/elm-datepicker)
