port module Main exposing (main)

{-| using [pd-andy/elm-web-audio](https://package.elm-lang.org/packages/pd-andy/elm-web-audio/latest/WebAudio-Program).
-}

import Browser
import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Json.Encode
import Random
import Time
import WebAudio as Audio
import WebAudio.Program as AudioProgram
import WebAudio.Property as AudioAttr


main : Program () Model Msg
main =
    AudioProgram.document
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , audio = play
        , audioPort = updateAudio
        }


type alias Model =
    { playing : Playing
    , song : Song
    , time : Int
    }


type Playing
    = Playing
    | Stopped


type alias Song =
    { tones : List Tone
    , duration : Int
    , name : String
    , seed : Maybe Int
    }


{-| A tone

  - `volume` is an ampllifier. Normal volume is 1, loud is 1.5, no volume is 0

-}
type alias Tone =
    { frequency : Float
    , duration : Int
    , begin : Int
    , volume : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { song = generateSong (Just initialSongSeed)
      , playing = Stopped
      , time = 0
      }
    , Cmd.none
    )


initialSongSeed : Int
initialSongSeed =
    0


type Msg
    = GenerateNewSong
    | Stop
    | Play
    | ToTime Int
    | UseSeed (Maybe Int)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GenerateNewSong ->
            ( model
            , Random.int Random.minInt Random.maxInt
                |> Random.generate (Just >> UseSeed)
            )

        Stop ->
            ( { model | playing = Stopped }
            , Cmd.none
            )

        Play ->
            ( { model | playing = Playing }
            , Cmd.none
            )

        ToTime newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        UseSeed seed ->
            ( { model
                | song = generateSong seed
                , time = 0
                , playing = Playing
              }
            , Cmd.none
            )


port updateAudio : Json.Encode.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    if
        model.playing
            == Playing
            && model.time
            < model.song.duration
    then
        Time.every 10 (\_ -> ToTime (model.time + 10))

    else
        Sub.none


type WordGroup
    = Verb
    | Person
    | Noun
    | Adjective
    | Other


generateSong : Maybe Int -> Song
generateSong seed =
    let
        randomWord : Random.Generator String
        randomWord =
            Random.uniform Verb [ Person, Noun, Adjective, Other ]
                |> Random.andThen
                    (\group ->
                        Random.uniform ""
                            (case group of
                                Person ->
                                    [ "you", "I", "me", "he", "she", "it", "they" ]

                                Verb ->
                                    [ "feel", "say", "see", "greed", "ride", "sink", "ride", "write", "whisper", "is", "are", "belong", "stay", "sit" ]

                                Noun ->
                                    [ "tree", "hous", "roof", "sea", "ocean", "sky", "window", "stone", "sand", "castle", "wind", "wound" ]

                                Adjective ->
                                    [ "drippy", "low", "high", "blue", "dark", "bright", "hidden", "distant", "soft", "silent", "sick", "windy" ]

                                Other ->
                                    [ "where", "no", "what", "–", "2", "how", "and", "&", "our", "my", "its", "their" ]
                            )
                    )
    in
    Random.int 10 (60 * 3)
        |> Random.andThen
            (\songSeconds ->
                Random.map2
                    (\tones name ->
                        { duration = songSeconds * 1000
                        , tones = tones
                        , name = name
                        , seed = seed
                        }
                    )
                    (Random.list (songSeconds * 2)
                        (Random.map4
                            (\frequency duration begin volume ->
                                { frequency = frequency
                                , duration =
                                    min duration (songSeconds * 1000 - begin)
                                , begin = begin
                                , volume = volume
                                }
                            )
                            (Random.float 80 800)
                            (Random.int 100 5000)
                            (Random.int 0 (songSeconds * 1000))
                            (Random.float 0.1 1.2)
                        )
                    )
                    (Random.list
                        (2 + (sqrt (toFloat songSeconds) / 4 |> round))
                        randomWord
                        |> Random.map (String.join " ")
                    )
            )
        |> (\generator ->
                Random.step generator
                    (Random.initialSeed
                        (seed |> Maybe.withDefault initialSongSeed)
                    )
                    |> Tuple.first
           )


view : Model -> Browser.Document Msg
view model =
    { title = "audize"
    , body =
        [ Ui.layoutWith
            { options =
                [ Ui.noHover
                , Ui.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            []
            (ui model)
        ]
    }


seedFromString : String -> Maybe Int
seedFromString string =
    case string of
        "" ->
            Nothing

        nonEmpty ->
            let
                transformLetter char =
                    if (char |> Char.toCode) > ('z' |> Char.toCode) then
                        'z' |> Char.toCode

                    else if (char |> Char.toCode) > ('z' |> Char.toCode) then
                        'a' |> Char.toCode

                    else
                        char |> Char.toCode
            in
            nonEmpty
                |> String.toLower
                |> String.toList
                |> List.indexedMap
                    (\i ch ->
                        transformLetter ch
                            * (26
                                ^ (String.length nonEmpty - i - 1)
                              )
                    )
                |> List.sum
                |> Just


seedToString : Maybe Int -> String
seedToString seed =
    case seed of
        Just int ->
            let
                letters intData =
                    Char.fromCode (Char.toCode 'a' + (intData |> modBy 26))
                        :: (case intData // 26 of
                                0 ->
                                    []

                                stillMore ->
                                    letters stillMore
                           )
            in
            letters int
                |> String.fromList

        Nothing ->
            ""


ui : Model -> Ui.Element Msg
ui model =
    Ui.column
        [ UiFont.family
            [ UiFont.typeface "Noto Sans" ]
        , UiBackground.color (Ui.rgb 0 0 0)
        , Ui.width Ui.fill
        , Ui.height Ui.fill
        ]
        [ Ui.el
            [ UiFont.light
            , UiFont.color (Ui.rgb 0 1 1)
            , UiFont.size 45
            , Ui.centerX
            , Ui.padding 20
            ]
            (Ui.text "audize")
        , Ui.row
            [ Ui.centerX
            , Ui.centerY
            ]
            [ UiInput.button
                [ UiBackground.color (Ui.rgba 0 0 0 0)
                , Ui.centerX
                , Ui.centerY
                ]
                { onPress = Just GenerateNewSong
                , label =
                    Ui.text "↻" |> Ui.el [ UiFont.size 80 ] |> input
                }
            , Ui.column [ Ui.width Ui.fill ]
                [ UiInput.text
                    [ UiFont.size 20
                    , UiFont.color (Ui.rgba 1 1 1 1)
                    , UiBackground.color (Ui.rgba 0 0 0 0)
                    , UiBorder.color (Ui.rgba 0 0 0 0)
                    ]
                    { onChange =
                        seedFromString >> UseSeed
                    , text =
                        model.song.seed
                            |> seedToString
                    , placeholder = Nothing
                    , label = UiInput.labelHidden "type the seed"
                    }
                , Ui.el
                    [ Ui.width Ui.fill
                    , Ui.height (Ui.px 2)
                    , Ui.centerY
                    , UiBackground.color inputColor
                    , UiBorder.rounded 2
                    ]
                    Ui.none
                ]
            ]
        , Ui.row
            [ UiFont.size 20
            , UiFont.color (Ui.rgba 1 1 1 0.6)
            , Ui.spacing 10
            , Ui.centerY
            , Ui.centerX
            ]
            [ UiInput.button []
                (let
                    { onPress, text } =
                        case model.playing of
                            Playing ->
                                { onPress = Stop
                                , text = "⏸"
                                }

                            Stopped ->
                                { onPress = Play
                                , text = "⏵"
                                }
                 in
                 { onPress = Just onPress
                 , label =
                    input (Ui.text text |> Ui.el [ UiFont.size 40 ])
                 }
                )
            , UiInput.slider
                [ Ui.width Ui.fill
                , Ui.behindContent
                    (Ui.el
                        [ Ui.width Ui.fill
                        , Ui.height (Ui.px 5)
                        , Ui.centerY
                        , UiBackground.color inputColor
                        , UiBorder.rounded 5
                        ]
                        Ui.none
                    )
                ]
                { onChange =
                    \newTime -> ToTime (round newTime)
                , label =
                    UiInput.labelBelow []
                        (formatMilliSeconds model.time
                            ++ " / "
                            ++ formatMilliSeconds model.song.duration
                            |> Ui.text
                        )
                , min = 0
                , max = toFloat model.song.duration
                , value = toFloat model.time
                , thumb =
                    UiInput.thumb
                        [ Ui.behindContent
                            (circle 14 (Ui.rgb 1 1 1)
                                |> Ui.el [ Ui.moveUp 6, Ui.moveLeft 2.5 ]
                            )
                        , UiBackground.color (Ui.rgba 0 0 0 0)
                        , UiBorder.color (Ui.rgba 0 0 0 0)
                        ]
                , step = Just 50
                }
            , Ui.text model.song.name |> Ui.el [ Ui.paddingXY 10 0 ]
            ]
        ]


circle r color =
    Ui.el
        [ Ui.width (Ui.px r)
        , Ui.height (Ui.px r)
        , UiBorder.rounded r
        , Ui.centerX
        , UiBackground.color color
        ]
        Ui.none


inputColor : Ui.Color
inputColor =
    Ui.rgb 1 0.6 0


input : Ui.Element msg -> Ui.Element msg
input ui_ =
    Ui.column
        [ UiFont.color (Ui.rgb 1 1 1)
        , Ui.padding 20
        , Ui.centerX
        ]
        [ ui_
        , circle 5 inputColor
        ]


formatMilliSeconds : Int -> String
formatMilliSeconds ms =
    let
        seconds =
            (ms // 1000) |> modBy 60
    in
    String.fromInt ((ms // 1000) // 60)
        ++ ":"
        ++ (if seconds < 10 then
                "0" ++ String.fromInt seconds

            else
                String.fromInt seconds
           )


play : Model -> Audio.Graph
play model =
    case model.playing of
        Stopped ->
            []

        Playing ->
            model.song.tones
                |> List.filterMap
                    (\{ frequency, duration, begin, volume } ->
                        let
                            playedFor =
                                model.time - begin
                        in
                        if
                            playedFor
                                >= 0
                                && playedFor
                                < duration
                        then
                            Audio.oscillator
                                [ AudioAttr.frequency frequency ]
                                [ Audio.gain
                                    [ AudioAttr.gain
                                        (1.8
                                            * volume
                                            * ((toFloat playedFor / toFloat duration) ^ 2.5)
                                            * ((toFloat (duration - playedFor) / toFloat duration) ^ 2.5)
                                        )
                                    ]
                                    [ Audio.audioDestination ]
                                ]
                                |> Just

                        else
                            Nothing
                    )
