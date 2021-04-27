port module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Element as El
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Set exposing (Set)


type alias OutgoingMsg =
    { toEval : String }


type alias IncomingMsg =
    { toEval : String, evalResult : Maybe String }


type Attempt
    = Unsolved
    | Solved String


type Input
    = Empty
    | HasInvalidCharacters String
    | WaitingForEval String
    | Evalled { toEval : String, evalResult : Maybe String }


type alias Model =
    { progress : Dict String Attempt
    , device : El.Device
    , input : Input
    }


type alias Flags =
    { height : Int
    , width : Int
    , solutions : List ( String, String )
    }


type Msg
    = None
    | TextChanged String
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
init { height, width, solutions } =
    let
        dict =
            Dict.fromList solutions
    in
    ( { progress =
            alphabet
                |> List.map
                    (\char ->
                        ( char
                        , case Dict.get char dict of
                            Just solution ->
                                Solved solution

                            Nothing ->
                                Unsolved
                        )
                    )
                |> Dict.fromList
      , device = El.classifyDevice { height = height, width = width }
      , input = Empty
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

        TextChanged toEval ->
            let
                valid =
                    String.all (\l -> Set.member l allowed) toEval
            in
            ( { model
                | input =
                    if toEval == "" then
                        Empty

                    else if valid then
                        WaitingForEval toEval

                    else
                        HasInvalidCharacters toEval
              }
            , if valid && toEval /= "" then
                OutgoingMsg toEval |> sendMessage

              else
                Cmd.none
            )

        EvalResultReceived ({ evalResult, toEval } as arg) ->
            let
                newModel =
                    { model
                        | input = Evalled arg
                    }
            in
            case evalResult of
                Nothing ->
                    ( newModel
                    , Cmd.none
                    )

                Just result ->
                    case Dict.get result model.progress of
                        Nothing ->
                            ( newModel, Cmd.none )

                        Just _ ->
                            ( { newModel
                                | progress = Dict.insert result (Solved toEval) model.progress
                              }
                            , Cmd.none
                            )


view : Model -> Html Msg
view model =
    model.progress
        |> Dict.toList
        |> List.map
            (\( char, attempt ) ->
                El.el
                    [ Bg.color
                        (if attempt == Unsolved then
                            El.rgba 0.8 0.8 0.8 0.5

                         else
                            El.rgba 0 1 0 0.5
                        )
                    , El.padding 15
                    , El.width (El.px 40)
                    ]
                    (El.text char)
            )
        |> (case ( model.device.class, model.device.orientation ) of
                ( El.Phone, El.Portrait ) ->
                    El.column

                _ ->
                    El.wrappedRow
           )
            [ El.spacing 50, El.padding 50 ]
        |> (\letters ->
                El.column []
                    [ letters
                    , Input.text
                        [ El.width (El.px 500), Font.family [ Font.monospace ], Input.focusedOnLoad ]
                        { onChange = TextChanged
                        , text = inputString model.input
                        , placeholder = Nothing
                        , label = Input.labelAbove [ El.centerX ] (El.text "Recreate the above only using !+()[]!")
                        }
                        |> El.el [ El.centerX ]
                    , El.el [ El.centerX ]
                        (El.text "→")
                    , El.el
                        [ El.centerX, Font.center, inputBackgroundColor model.input, El.paddingXY 50 30, El.height <| El.px 90, El.width <| El.px 500 ]
                        ((case model.input of
                            Empty ->
                                ""

                            HasInvalidCharacters _ ->
                                "Disallowed characters used!"

                            WaitingForEval _ ->
                                "Processing..."

                            Evalled { evalResult } ->
                                case evalResult of
                                    Nothing ->
                                        "Opps! JS error"

                                    Just string ->
                                        string
                         )
                            |> El.text
                        )
                    ]
           )
        |> El.layout []


inputBackgroundColor input =
    case input of
        Empty ->
            El.rgba 0.9 0.9 0.9 0.5 |> Bg.color

        HasInvalidCharacters _ ->
            El.rgba 1 0 0 0.5 |> Bg.color

        WaitingForEval _ ->
            El.rgba 0.9 0.9 0.9 0.5 |> Bg.color

        Evalled { evalResult } ->
            Bg.color <|
                case evalResult of
                    Nothing ->
                        El.rgba 1 0 0 0.5

                    Just _ ->
                        El.rgba 0 1 0 0.5


inputString : Input -> String
inputString input =
    case input of
        Empty ->
            ""

        HasInvalidCharacters string ->
            string

        WaitingForEval string ->
            string

        Evalled { toEval } ->
            toEval
