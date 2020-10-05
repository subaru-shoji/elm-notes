module Main exposing (main)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography as Typography exposing (textColor)
import Html exposing (Html, div, i, main_, text)
import Html.Attributes exposing (class, href, placeholder, rel, style, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Note =
    { id : Id
    , title : String
    , editTime : Time.Posix
    , body : String
    }


type alias Model =
    { notes : List Note
    , userName : UserName
    , activeNoteId : Id
    , nextNoteId : Id
    , zone : Time.Zone
    , currentTime : Time.Posix
    }


type alias Id =
    Int


type alias UserName =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes =
            [ Note 1 "銀河鉄道の夜" (Time.millisToPosix 1601478000) "なんたらかんたら"
            , Note 2 "銀河鉄道の夜2" (Time.millisToPosix 1601478000) "やばい"
            , Note 3 "銀河鉄道の夜3" (Time.millisToPosix 1601478000) "すごい"
            ]
      , userName = "John Doe"
      , activeNoteId = 1
      , nextNoteId = 4
      , zone = Time.utc
      , currentTime = Time.millisToPosix 0
      }
    , Task.perform AdjustTimeZone Time.here
    )


type Msg
    = AddNewNote
    | ChangeActiveNoteId Id
    | EditNoteTitle Id String
    | EditNoteBody Id String
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddNewNote ->
            let
                addNoteTimeString =
                    timeToDateString model.currentTime model.zone
            in
            ( { model
                | notes =
                    List.append model.notes
                        [ Note model.nextNoteId "" model.currentTime "" ]
                , activeNoteId = model.nextNoteId
                , nextNoteId = model.nextNoteId + 1
              }
            , Cmd.none
            )

        ChangeActiveNoteId nextId ->
            ( { model | activeNoteId = nextId }, Cmd.none )

        EditNoteTitle id title ->
            let
                newNotes =
                    List.map
                        (\note ->
                            if note.id == id then
                                { note | title = title }

                            else
                                note
                        )
                    <|
                        model.notes
            in
            ( { model | notes = newNotes }, Cmd.none )

        EditNoteBody id body ->
            let
                newNotes =
                    List.map
                        (\note ->
                            if note.id == id then
                                { note | body = body }

                            else
                                note
                        )
                    <|
                        model.notes
            in
            ( { model | notes = newNotes }, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


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


view : Model -> Html Msg
view model =
    main_ [ style "background-color" "black" ]
        [ stylesheet
        , fontAwesomeCDN
        , baseColumns model
        ]


fontAwesomeCDN : Html msg
fontAwesomeCDN =
    Html.node "link"
        [ rel "stylesheet"
        , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
        ]
        []


getActiveNote : List Note -> Id -> Note
getActiveNote notes id =
    notes
        |> List.filter (\note -> note.id == id)
        |> List.head
        |> Maybe.withDefault notFoundNote


notFoundNote : Note
notFoundNote =
    -- ありえないので適当
    { id = 9999
    , title = "not found"
    , editTime = Time.millisToPosix 0
    , body = "not found"
    }


baseColumns : Model -> Html Msg
baseColumns model =
    columns columnsModifiers
        [ style "max-height" "100vh"
        ]
        [ column columnModifiers
            [ class "has-background-primary"
            , class "is-one-fifth"
            ]
            [ sideColumn model.userName
            ]
        , column columnModifiers
            [ class "has-background-light"
            , style "overflow-y" "scroll"
            ]
            [ notesColumn model ]
        , column columnModifiers
            [ style "background-color" "white" ]
            [ editColumn (getActiveNote model.notes model.activeNoteId)
            ]
        ]


sideColumn : UserName -> Html Msg
sideColumn userName =
    div
        [ style "padding" "0.2rem 0 1.0rem 0.55rem"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style
            "min-height"
            "98vh"
        ]
        [ title H1 [] [ text "NoteApp" ]
        , title H4
            [ style "margin-top" "auto"
            ]
            [ text userName
            , text " "
            , icon Standard
                [ textColor Typography.Black ]
                [ i [ class "fa fa-cog" ] [] ]
            ]
        ]


notesColumn : Model -> Html Msg
notesColumn model =
    let
        createNoteRow : Note -> Html Msg
        createNoteRow note =
            box [ onClick (ChangeActiveNoteId note.id) ]
                [ if note.title == "" then
                    div [ class "has-text-grey" ] [ text "No title" ]

                  else
                    div [] [ text note.title ]
                , div [] [ text (timeToDateString note.editTime model.zone) ]
                , div [] [ text note.body ]
                ]
    in
    div []
        (List.append
            [ button buttonModifiers
                [ onClick AddNewNote
                , style "margin" "1rem 0"
                ]
                [ text "add Note" ]
            ]
            (List.map
                createNoteRow
             <|
                model.notes
            )
        )


editColumn : Note -> Html Msg
editColumn note =
    div []
        [ controlText controlInputModifiers
            []
            [ value note.title
            , placeholder "Input"
            , onInput (EditNoteTitle note.id)
            ]
            []
        , controlTextArea controlTextAreaModifiers
            []
            [ value note.body
            , placeholder "Input body"
            , style "height" "93vh"
            , style "max-height" "93vh"
            , onInput (EditNoteBody note.id)
            ]
            []
        ]
