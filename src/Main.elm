port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Element as El
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Set exposing (Set)


type alias OutgoingMsg =
    { letter : String, solution : String }


type alias IncomingMsg =
    { letter : String, solution : String, result : Bool }


type Attempt
    = Unattempted
    | HasInvalidCharacters String
    | WaitingForEval String
    | Correct String
    | Incorrect String


type alias Model =
    { progress : Dict String Attempt
    , device : El.Device
    }


type alias Flags =
    { height : Int
    , width : Int
    }


type Msg
    = None
    | TextChanged { for : String, newValue : String }
    | EvalResultReceived IncomingMsg


alphabet : List String
alphabet =
    "abcdefghijklmnopqrstuvwxyz" |> String.toList |> List.map String.fromChar


allowed : Set Char
allowed =
    "()+[]!" |> String.toList |> Set.fromList


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = messageReceiver EvalResultReceived |> always
        }


init : Flags -> ( Model, Cmd msg )
init { height, width } =
    ( { progress =
            alphabet
                |> List.map (\char -> ( char, Unattempted ))
                |> Dict.fromList
      , device = El.classifyDevice { height = height, width = width }
      }
    , Cmd.none
    )



-- PORTS


port sendMessage : OutgoingMsg -> Cmd msg


port messageReceiver : (IncomingMsg -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        TextChanged { for, newValue } ->
            let
                valid =
                    String.all (\l -> Set.member l allowed) newValue
            in
            ( { model
                | progress =
                    model.progress
                        |> Dict.insert for
                            (if newValue == "" then
                                Unattempted

                             else if valid then
                                WaitingForEval newValue

                             else
                                HasInvalidCharacters newValue
                            )
              }
            , if valid && newValue /= "" then
                OutgoingMsg for newValue |> sendMessage

              else
                Cmd.none
            )

        EvalResultReceived { letter, result, solution } ->
            ( { model
                | progress =
                    model.progress
                        |> Dict.insert letter
                            ((if result then
                                Correct

                              else
                                Incorrect
                             )
                                solution
                            )
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    model.progress
        |> Dict.toList
        |> List.map
            (\( char, attempt ) ->
                Input.text [ El.width (El.px 300), El.alignLeft, Font.family [ Font.monospace ] ]
                    { onChange = \newValue -> TextChanged { for = char, newValue = newValue }
                    , text = attemptString attempt
                    , placeholder = Nothing
                    , label = Input.labelLeft (El.width (El.px 20) :: labelBorder attempt) (El.text char)
                    }
            )
        |> (case ( model.device.class, model.device.orientation ) of
                ( El.Phone, El.Portrait ) ->
                    El.column

                _ ->
                    El.wrappedRow
           )
            [ El.spacing 50, El.padding 50 ]
        |> El.layout []


attemptString : Attempt -> String
attemptString attempt =
    case attempt of
        Unattempted ->
            ""

        WaitingForEval string ->
            string

        Correct string ->
            string

        Incorrect string ->
            string

        HasInvalidCharacters string ->
            string


labelBorder : Attempt -> List (El.Attribute msg)
labelBorder attempt =
    case attempt of
        Correct _ ->
            [ El.rgb 0 1 0 |> Border.color, Border.width 1 ]

        Incorrect _ ->
            [ El.rgb 1 0 0 |> Border.color, Border.width 1 ]

        Unattempted ->
            [ Border.width 0 ]

        WaitingForEval _ ->
            [ El.rgb 0.5 0.5 0.5 |> Border.color, Border.width 1 ]

        HasInvalidCharacters _ ->
            [ El.rgb255 255 165 0 |> Border.color, Border.width 1 ]
