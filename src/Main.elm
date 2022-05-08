module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, node, span, text)
import Html.Attributes exposing (class, classList, href, rel, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)



---- MODEL ----


type Model
    = Loading
    | Error
    | Running GameState


type alias GameState =
    { correctGuesses : Int
    , hasLost : Bool
    , hasWon : Bool
    , incorrectGuesses : Int
    , lettersGuessed : Set String
    , maxCorrectGuesses : Int
    , showHint : Bool
    , wordData : WordData
    }


type alias WordData =
    { word : String
    , hint : String
    }



-- Initial state


init : ( Model, Cmd Msg )
init =
    ( Loading
    , fetchWordData
    )



---- UPDATE ----


type Msg
    = Guess String
    | Restart
    | ShowHint
    | NewWord (Result Http.Error WordData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Guess char ->
            case model of
                Running gameState ->
                    let
                        correctGuess =
                            if (String.contains char gameState.wordData.word) && 
                                (not <| Set.member char gameState.lettersGuessed ) then
                                1

                            else
                                0

                        incorrectGuess =
                            if not <| String.contains char gameState.wordData.word then
                                1

                            else
                                0
                        hasWon =
                            gameState.correctGuesses + correctGuess >= gameState.maxCorrectGuesses

                        hasLost =
                            gameState.incorrectGuesses + incorrectGuess >= 6
                    in
                    if hasWon then
                        ( Running
                            { gameState
                                | hasWon = True
                                , correctGuesses = String.length gameState.wordData.word
                                , lettersGuessed = Set.insert char gameState.lettersGuessed
                            }
                        , Cmd.none
                        )

                    else if hasLost then
                        ( Running
                            { gameState
                                | hasLost = True
                                , incorrectGuesses = 6
                                , lettersGuessed = Set.insert char gameState.lettersGuessed
                            }
                        , Cmd.none
                        )

                    else
                        ( Running
                            { gameState
                                | lettersGuessed = Set.insert char gameState.lettersGuessed
                                , correctGuesses = gameState.correctGuesses + correctGuess
                                , incorrectGuesses = gameState.incorrectGuesses + incorrectGuess
                            }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        Restart ->
            ( Loading, fetchWordData )

        ShowHint ->
            case model of
                Running gameState ->
                    ( Running { gameState | showHint = True }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NewWord result ->
            case result of
                Ok data ->
                    let
                        maxCorrectGuesses = 
                            data.word
                                |> String.split ""
                                |> Set.fromList
                                |> Set.size
                    in
                    ( Running
                        { correctGuesses = 0
                        , hasLost = False
                        , hasWon = False
                        , incorrectGuesses = 0
                        , lettersGuessed = Set.empty
                        , maxCorrectGuesses = maxCorrectGuesses
                        , showHint = False
                        , wordData = data
                        }
                    , Cmd.none
                    )

                Err _ ->
                    ( Error, Cmd.none )


fetchWordData : Cmd Msg
fetchWordData =
    Http.get
        { url = "https://devitar-api.glitch.me/random-word"
        , expect = Http.expectJson NewWord wordDecoder
        }


wordDecoder : Decoder WordData
wordDecoder =
    Decode.map2 WordData
        (Decode.field "word" Decode.string)
        (Decode.field "hint" Decode.string)



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [ class "loading-view" ]
                [ text "Loading (This may take a minute if this is your first time playing...)"
                , div [ class "loading-icon" ] [ text "↻" ]
                ]

        Running gameState ->
            viewGameState gameState

        Error ->
            div [] [ text "Oops - something went wrong. Sorry about that!" ]


viewGameState : GameState -> Html Msg
viewGameState gameState =
    let
        wordSet =
            gameState.wordData.word
                |> String.split ""
                |> Set.fromList

        badLettersList =
            gameState.lettersGuessed
                |> Set.toList
                |> List.filter
                    (\char ->
                        not <| Set.member char wordSet
                    )

        goodLettersList =
            gameState.lettersGuessed
                |> Set.toList
                |> List.filter
                    (\char ->
                        Set.member char wordSet
                    )

        shouldDisplayOutcome =
            gameState.hasWon || gameState.hasLost

        winLossModalHtml =
            div
                [ classList
                    [ ( "win-loss-modal", True )
                    , ( "displayed", shouldDisplayOutcome )
                    ]
                ]
                [ text <|
                    if gameState.hasWon then
                        "You won! The word was: " ++ gameState.wordData.word

                    else if gameState.hasLost then
                        "You lost! The word was: " ++ gameState.wordData.word

                    else
                        "If you can see this, something has gone horribly wrong..."
                , button
                    [ onClick Restart
                    , class "action-button"
                    ]
                    [ text "⟲ Try again?" ]
                ]

        wordHtml =
            gameState.wordData.word
                |> String.split ""
                |> List.map
                    (\char ->
                        if char == " " then
                            " "

                        else if Set.member char gameState.lettersGuessed then
                            char

                        else
                            " "
                    )
                |> List.map
                    (\char ->
                        span
                            [ class "character-display" ]
                            [ text char ]
                    )
                |> div [ class "letter-container" ]

        hangmanImagesHtml =
            case List.length badLettersList of
                0 ->
                    img
                        [ class "hangman-image"
                        , src "assets/game_background.png"
                        ]
                        []

                1 ->
                    img
                        [ class "hangman-image"
                        , src "assets/game_background1.png"
                        ]
                        []

                2 ->
                    img
                        [ class "hangman-image"
                        , src "assets/game_background2.png"
                        ]
                        []

                3 ->
                    img
                        [ class "hangman-image"
                        , src "assets/game_background3.png"
                        ]
                        []

                4 ->
                    img
                        [ class "hangman-image"
                        , src "assets/game_background4.png"
                        ]
                        []

                5 ->
                    img
                        [ class "hangman-image"
                        , src "assets/game_background5.png"
                        ]
                        []

                6 ->
                    img
                        [ class "hangman-image"
                        , src "assets/game_background6.png"
                        ]
                        []

                _ ->
                    img
                        [ class "hangman-image"
                        , src "assets/game_background6.png"
                        ]
                        []

        buttonsHtml =
            "abcdefghijklmnopqrstuvwxyz"
                |> String.split ""
                |> List.map
                    (\char ->
                        button
                            [ onClick <| Guess char
                            , classList
                                [ ( "guess-button", True )
                                , ( "incorrect", List.member char badLettersList )
                                , ( "correct", List.member char goodLettersList )
                                ]
                            ]
                            [ text char ]
                    )
                |> div []

        hintHtml =
            if gameState.showHint == True then
                div [ class "hint-text" ] [ text <| "Hint: " ++ gameState.wordData.hint ]

            else
                button
                    [ onClick ShowHint
                    , class "action-button"
                    ]
                    [ text "Show hint" ]
    in
    div [ class "view" ]
        [ hangmanImagesHtml
        , wordHtml
        , hintHtml
        , buttonsHtml
        , button
            [ onClick Restart
            , class "action-button"
            ]
            [ text "⟲ Restart" ]
        , winLossModalHtml
        , img 
            [ class "background-image"
            , src "assets/paper_texture.jpg"
            ]
            []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
