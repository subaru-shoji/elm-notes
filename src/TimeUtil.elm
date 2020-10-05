module TimeUtil exposing (timeToDateString)

import Time


timeToDateString : Time.Posix -> Time.Zone -> String
timeToDateString time zone =
    let
        year =
            String.fromInt (Time.toYear zone time)

        month =
            Time.toMonth zone time |> toMonthString

        day =
            String.fromInt (Time.toDay zone time) |> String.pad 2 '0'
    in
    year ++ "/" ++ month ++ "/" ++ day


toMonthString : Time.Month -> String
toMonthString month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"
