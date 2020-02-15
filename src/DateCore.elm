module DateCore exposing
    ( Year
    , datesOfMonth
    , equal
    , getFormattedDate
    , getYearAndMonthNext
    , getYearAndMonthPrevious
    , greaterOrEqual
    , groupByWeek
    , inRange
    , lowerOrEqual
    , monthToString
    , nothingToMonday
    , nothingToSunday
    )

import Date exposing (Date)
import Time exposing (Month(..))


type alias Year =
    Int


isLeapYear : Year -> Bool
isLeapYear year =
    ((modBy 4 year == 0) && (modBy 100 year /= 0)) || (modBy 400 year == 0)


daysInMonth : Year -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


datesOfMonth : Year -> Month -> List Date
datesOfMonth year month =
    List.range 1 (daysInMonth year month)
        |> List.map (Date.fromCalendarDate year month)


monthToString : Month -> String
monthToString month =
    Date.fromCalendarDate 2018 month 1
        |> Date.format "MMM"


groupByWeek : List (Maybe Date) -> List (List (Maybe Date))
groupByWeek l =
    let
        group res tail =
            case tail of
                [] ->
                    List.reverse res

                _ ->
                    group (List.take 7 tail :: res) (List.drop 7 tail)
    in
    group [] l


getFormattedDate : Maybe Date -> String
getFormattedDate =
    Maybe.map (Date.day >> String.fromInt)
        >> Maybe.withDefault ""


getYearAndMonthNext : Year -> Month -> ( Year, Month )
getYearAndMonthNext year month =
    addMonths 1 ( year, month )


getYearAndMonthPrevious : Year -> Month -> ( Year, Month )
getYearAndMonthPrevious year month =
    addMonths -1 ( year, month )


addMonths : Int -> ( Year, Month ) -> ( Year, Month )
addMonths numMonthsToAdd ( year, month ) =
    Date.fromCalendarDate year month 1
        |> Date.add Date.Months numMonthsToAdd
        |> (\date -> ( Date.year date, Date.month date ))


nothingToMonday : Maybe Date -> List (Maybe Date)
nothingToMonday date =
    case date of
        Just d ->
            List.repeat (Date.weekdayNumber d - 1) Nothing

        Nothing ->
            []


nothingToSunday : Maybe Date -> List (Maybe Date)
nothingToSunday date =
    case date of
        Just d ->
            List.repeat (7 - Date.weekdayNumber d) Nothing

        Nothing ->
            []


compareMaybeDate : Maybe Date -> Maybe Date -> Order
compareMaybeDate date1 date2 =
    case ( date1, date2 ) of
        ( Nothing, Just _ ) ->
            LT

        ( Nothing, Nothing ) ->
            EQ

        ( Just _, Nothing ) ->
            GT

        ( Just d1, Just d2 ) ->
            compare (Date.toRataDie d1) (Date.toRataDie d2)


equal : Maybe Date -> Maybe Date -> Bool
equal date1 date2 =
    compareMaybeDate date1 date2 == EQ


lowerOrEqual : Maybe Date -> Maybe Date -> Bool
lowerOrEqual d1 d2 =
    List.member (compareMaybeDate d1 d2) [ LT, EQ ]


greater : Maybe Date -> Maybe Date -> Bool
greater d1 d2 =
    compareMaybeDate d1 d2 == GT


greaterOrEqual : Maybe Date -> Maybe Date -> Bool
greaterOrEqual d1 d2 =
    List.member (compareMaybeDate d1 d2) [ EQ, GT ]


inRange : Maybe Date -> Maybe Date -> Maybe Date -> Bool
inRange date start end =
    greaterOrEqual date start && lowerOrEqual date end
