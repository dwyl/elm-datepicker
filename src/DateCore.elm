module DateCore exposing (..)

import Date exposing (..)


type alias Year =
    Int


isLeapYear : Int -> Bool
isLeapYear year =
    ((year % 4 == 0) && (year % 100 /= 0)) || ((year % 400) == 0)


indexOfDay : Day -> Int
indexOfDay day =
    case day of
        Mon ->
            0

        Tue ->
            1

        Wed ->
            2

        Thu ->
            3

        Fri ->
            4

        Sat ->
            5

        Sun ->
            6


daysInMonth : Int -> Date.Month -> Int
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


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


monthFromInt : Int -> Maybe Month
monthFromInt month =
    case month of
        1 ->
            Just Jan

        2 ->
            Just Feb

        3 ->
            Just Mar

        4 ->
            Just Apr

        5 ->
            Just May

        6 ->
            Just Jun

        7 ->
            Just Jul

        8 ->
            Just Aug

        9 ->
            Just Sep

        10 ->
            Just Oct

        11 ->
            Just Nov

        12 ->
            Just Dec

        _ ->
            Nothing


groupByWeek : List (Maybe Date) -> List (List (Maybe Date))
groupByWeek l =
    let
        group res tail =
            case tail of
                [] ->
                    List.reverse res

                _ ->
                    group ((List.take 7 tail) :: res) (List.drop 7 tail)
    in
        group [] l


getFormattedDate : Maybe Date -> String
getFormattedDate date =
    case date of
        Just d ->
            toString <| Date.day d

        Nothing ->
            ""


formatDatePill : Date -> String
formatDatePill date =
    (toString (Date.day date)) ++ " " ++ (toString (Date.month date))


getYearAndMonth : Maybe Date -> ( Year, Month )
getYearAndMonth date =
    case date of
        Just d ->
            ( Date.year d, Date.month d )

        Nothing ->
            ( 2018, Jan )


get18monthDate : Maybe Date -> Maybe Date
get18monthDate date =
    case date of
        Just d ->
            let
                totalMonths =
                    (monthToInt (Date.month d)) + 6

                year =
                    (Date.year d) + (totalMonths // 12) + 1

                monthIndex =
                    totalMonths % 12

                month =
                    case (monthFromInt monthIndex) of
                        Just m ->
                            m

                        Nothing ->
                            Dec

                lastDay =
                    daysInMonth year month

                formattedDate =
                    (toString year) ++ "-" ++ (toString month) ++ "-" ++ (toString lastDay)
            in
                case (Date.fromString formattedDate) of
                    Ok d ->
                        Just d

                    Err _ ->
                        Nothing

        Nothing ->
            Nothing


getNextDay : Maybe Date -> Maybe Date
getNextDay date =
    case date of
        Just d ->
            let
                nextDay =
                    Date.fromTime ((Date.toTime d) + (24 * 60 * 60 * 1000))
            in
                Just nextDay

        Nothing ->
            Nothing


getYearAndMonthNext : Year -> Month -> ( Year, Month )
getYearAndMonthNext year month =
    let
        monthIndex =
            monthToInt month

        nextMonthIndex =
            monthFromInt (monthIndex + 1)
    in
        case (monthIndex) of
            12 ->
                ( year + 1, Jan )

            _ ->
                case nextMonthIndex of
                    Just m ->
                        ( year, m )

                    Nothing ->
                        ( year, Jan )


getYearAndMonthPrevious : Year -> Month -> ( Year, Month )
getYearAndMonthPrevious year month =
    let
        monthIndex =
            monthToInt month

        prevMonthIndex =
            monthFromInt (monthIndex - 1)
    in
        case monthIndex of
            1 ->
                ( year - 1, Dec )

            _ ->
                case prevMonthIndex of
                    Just m ->
                        ( year, m )

                    Nothing ->
                        ( year, Jan )


dateFromString : String -> Maybe Date
dateFromString s =
    case Date.fromString s of
        Ok d ->
            Just d

        Err _ ->
            Nothing


formatDate : Int -> Int -> Int -> String
formatDate year month day =
    toString year ++ "-" ++ formatWithZero (toString month) ++ "-" ++ formatWithZero (toString day) ++ "T00:00:00"


formatWithZero : String -> String
formatWithZero date =
    if String.length date == 1 then
        "0" ++ date
    else
        date


dateToSring : Date -> String
dateToSring date =
    formatDate (Date.year date) (monthToInt (Date.month date)) (Date.day date)


toDate : Int -> Int -> Int -> Maybe Date
toDate year month day =
    dateFromString <| formatDate year month day


nothingToMonday : Maybe (Maybe Date) -> List (Maybe Date)
nothingToMonday date =
    case date of
        Just (Just d) ->
            List.repeat (indexOfDay (Date.dayOfWeek d)) Nothing

        Just Nothing ->
            []

        Nothing ->
            []


nothingToSunday : Maybe (Maybe Date) -> List (Maybe Date)
nothingToSunday date =
    case date of
        Just (Just d) ->
            let
                reps =
                    6 - (indexOfDay (Date.dayOfWeek d))
            in
                List.repeat reps Nothing

        Just Nothing ->
            []

        Nothing ->
            []


toTime : Date -> Int
toTime =
    floor << Date.toTime


equal : Date -> Date -> Bool
equal a b =
    toTime a == toTime b


lowerOrEqual : Maybe Date -> Maybe Date -> Bool
lowerOrEqual d1 d2 =
    case ( d1, d2 ) of
        ( Nothing, _ ) ->
            True

        ( _, Nothing ) ->
            False

        ( Just date1, Just date2 ) ->
            (toTime date1) <= (toTime date2)


greater : Maybe Date -> Maybe Date -> Bool
greater d1 d2 =
    case ( d1, d2 ) of
        ( _, Nothing ) ->
            True

        ( Just date1, Just date2 ) ->
            (toTime date1) > (toTime date2)

        ( Nothing, Just _ ) ->
            False


greaterOrEqual : Maybe Date -> Maybe Date -> Bool
greaterOrEqual d1 d2 =
    case ( d1, d2 ) of
        ( _, Nothing ) ->
            True

        ( Just date1, Just date2 ) ->
            (toTime date1) >= (toTime date2)

        ( Nothing, Just _ ) ->
            False


inRange : Maybe Date -> Maybe Date -> Maybe Date -> Bool
inRange d d1 d2 =
    if (greaterOrEqual d d1) && (lowerOrEqual d d2) then
        True
    else
        False
