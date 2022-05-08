module Main exposing (..)

import Browser
import Html exposing (Html, button, div, img, node, span, text)
import Html.Attributes exposing (class, classList, href, rel)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)



---- MODEL ----


type Model
    = Loading
    | Running GameState
    | Error


type alias GameState =
    { wordData : WordData
    , guesses : Set String
    , showHint : Bool
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
                    ( Running { gameState | guesses = Set.insert char gameState.guesses }, Cmd.none )

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
                    ( Running { wordData = data, guesses = Set.empty, showHint = False }, Cmd.none )

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
            gameState.guesses
                |> Set.toList
                |> List.filter
                    (\char ->
                        not <| Set.member char wordSet
                    )

        goodLettersList =
            gameState.guesses
                |> Set.toList
                |> List.filter
                    (\char ->
                        Set.member char wordSet
                    )

        wordHtml =
            gameState.wordData.word
                |> String.split ""
                |> List.map
                    (\char ->
                        if char == " " then
                            " "

                        else if Set.member char gameState.guesses then
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
                        [ class "hangman-image" ]
                        []

                1 ->
                    img
                        [ class "hangman-image i1" ]
                        []

                2 ->
                    img
                        [ class "hangman-image i2" ]
                        []

                3 ->
                    img
                        [ class "hangman-image i3" ]
                        []

                4 ->
                    img
                        [ class "hangman-image i4" ]
                        []

                5 ->
                    img
                        [ class "hangman-image i5" ]
                        []

                6 ->
                    img
                        [ class "hangman-image i6" ]
                        []

                _ ->
                    img
                        [ class "hangman-image i6" ]
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
        , node "link"
            [ href "https://fonts.googleapis.com/css2?family=Handlee&display=swap"
            , rel "stylesheet"
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
