# Elm DatePicker

A customisable Datepicker written in Elm.

[![Build Status](https://travis-ci.org/dwyl/elm-datepicker.svg?branch=master)](https://travis-ci.org/dwyl/elm-datepicker)

## Demo/Example: https://elm-datepicker.herokuapp.com/#Styled

![demo-screenshot](https://user-images.githubusercontent.com/194400/45051747-d9203500-b07c-11e8-9408-a245c8f81363.png)

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

To run the examples on your `localhost`, run the following:

<!-- these instructions will need updating for v0.19 see: https://github.com/dwyl/elm-datepicker/issues/32 -->
```sh
git clone https://github.com/dwyl/elm-datepicker.git && cd elm-datepicker
elm-package install -y
npm install
elm make examples/*.elm --output=examples/example.js
elm-reactor
```

Then visit `localhost:8000` and open `index.html`

Alternatively, the examples are hosted on Heroku: https://elm-datepicker.herokuapp.com/#Styled

### Simple DatePicker

<img width="496" alt="simple datepicker example" src="https://user-images.githubusercontent.com/8939909/39987635-3e3695ca-575d-11e8-82b1-96c6da221c02.png">

This example shows the basic datepicker implementation, with no additional styling, only the addition of buttons to move to the next/previous month. [See code](https://github.com/dwyl/elm-datepicker/blob/master/examples/Simple.elm)

### Styled DatePicker

<img width="496" alt="styled datepicker example" src="https://user-images.githubusercontent.com/8939909/39988285-dfec33b0-575e-11e8-8e9c-7db2bb9b9162.png">

The basic implementation of the datepicker with some custom styling applied. [See code](https://github.com/dwyl/elm-datepicker/blob/master/examples/Styled.elm)

### Range DatePicker

<img width="496" alt="range datepicker example" src="https://user-images.githubusercontent.com/8939909/39988708-f8afd450-575f-11e8-8f56-083804b146e6.png">

The basic implementation of the datepicker, initialised to select a range instead of a single date. [See code](https://github.com/dwyl/elm-datepicker/blob/master/examples/Range.elm)

### Two Month DatePicker

<img width="496" alt="two month datepicker example" src="https://user-images.githubusercontent.com/8939909/39988831-5068ce2c-5760-11e8-9cb4-b7a3a3e482b3.png">

An example showing how to display two months in one datepicker. [See code](https://github.com/dwyl/elm-datepicker/blob/master/examples/TwoMonth.elm)

### Dropdown DatePicker

<img width="496" alt="dropdown datepicker example" src="https://user-images.githubusercontent.com/8939909/39988995-bafb319e-5760-11e8-8f83-90877cd788e4.png">

An example showing how to toggle the visibility of the datepicker, as well as how to use the selected date outside of the datepicker. [See code](https://github.com/dwyl/elm-datepicker/blob/master/examples/Dropdown.elm)

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
