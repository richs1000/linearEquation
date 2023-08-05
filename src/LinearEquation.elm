port module LinearEquation exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, style, title, type_)
import Html.Events exposing (onClick)
import Random
import Round


port getFromTorus : (Flags -> msg) -> Sub msg


port sendToTorus : Bool -> Cmd msg


type Answer
    = NumberChoice Float -- when the answers are a single number (e.g., what's the slope?)
    | ScatterPlotChoice Float Float Float -- Ax^2 + Bx + C when the answers require an equation (e.g., which graph?)


type Question
    = WhatIsTheSlope Float Float -- given an equation, what is the slope?
    | WhatIsTheIntercept Float Float -- given an equation, what is the y-intercept?
    | WhichGraph Float Float -- which graph corresponds to this equation?
    | WhatIsY Float Float Int -- given an equation and x, what is the value of y?


type RightOrWrong
    = RightAnswer -- user chose the correct answer
    | WrongAnswer -- user chose the wrong answer
    | NothingYet -- this is used by progress bar when the window is bigger than the number of responses


type Status
    = WaitingToStart -- user needs to ask for next question
    | WaitingForAnswer -- user needs to choose an answer
    | GotAnswer RightOrWrong -- user has answered, so now we need to give feedback


type alias Model =
    { question : Question -- What question do we show to the user?
    , progress : List RightOrWrong -- How many questions has the user gotten right/wrong?
    , status : Status -- What should we be showing to the user?
    , threshold : Int -- How many questions does the user need to get right?
    , window : Int -- How big is the window for reaching the threshold?
    , debug : Bool -- Do we show debug information?
    }


initialModel : Model
initialModel =
    { question = WhatIsTheSlope 2 2
    , progress = List.repeat 6 NothingYet
    , status = WaitingToStart
    , threshold = 4
    , window = 6
    , debug = True
    }


type Msg
    = GetNextQuestion -- Generate random numbers for the next question
    | GotRandomQuestion Question -- Use random numbers to create and display next question
    | GotResponse RightOrWrong -- Give feedback to user about their answer
    | ReturnToTorus -- The user reached the threshold, go back to Torus (send to JavaScript)
    | GetDataFromTorus Flags -- Data coming in from Torus (get from JavaScript)


type alias Flags =
    { threshold : Int -- User needs to get <threshold> questions right...
    , window : Int -- out of the last <window> questions
    , debug : Bool -- True when we should show debug info
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Linear Equations" ]
        , viewQuestionPanel model
        , viewFeedbackPanel model
        , viewButtonPanel model
        , viewProgressPanel model
        , viewDebugPanel model
        ]


viewQuestionPanel : Model -> Html Msg
viewQuestionPanel model =
    case model.status of
        WaitingToStart ->
            div [ id "questionPanel" ] []

        WaitingForAnswer ->
            viewQuestion model

        GotAnswer _ ->
            div [ id "questionPanel" ] []


viewQuestion : Model -> Html Msg
viewQuestion model =
    div [ id "questionPanel" ]
        [ text "If this is your equation:"
        , questionText model.question
        ]


questionText : Question -> Html Msg
questionText quest =
    case quest of
        WhatIsTheSlope slope yIntercept ->
            div [ id "questionText" ]
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "What is the slope?"
                ]

        WhatIsTheIntercept slope yIntercept ->
            div [ id "questionText" ]
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "What is the y-intercept?"
                ]

        WhichGraph slope yIntercept ->
            div [ id "questionText" ]
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "Which graph corresponds to this equation?"
                ]

        WhatIsY slope yIntercept x ->
            div [ id "questionText" ]
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text ("If x = " ++ String.fromInt x ++ "What does y equal?")
                ]


equationAsString : Float -> Float -> String
equationAsString slope yIntercept =
    let
        operator =
            if yIntercept < 0 then
                "- "

            else
                "+ "
    in
    "y = "
        ++ String.fromFloat slope
        ++ "x "
        ++ operator
        ++ String.fromFloat (abs yIntercept)


viewFeedbackPanel : Model -> Html Msg
viewFeedbackPanel model =
    case model.status of
        WaitingToStart ->
            div [ id "feedbackPanel" ] []

        WaitingForAnswer ->
            div [ id "feedbackPanel" ] [ text "Choose the correct answer" ]

        GotAnswer RightAnswer ->
            div [ id "feedbackPanel" ] [ text "Correct!" ]

        GotAnswer WrongAnswer ->
            div [ id "feedbackPanel" ] [ text "Incorrect" ]

        GotAnswer NothingYet ->
            div [ id "feedbackPanel" ] [ text "Nothing Yet" ]


crossedThreshold : Model -> Bool
crossedThreshold model =
    let
        sumRightAnswers =
            List.filter (\p -> p == RightAnswer) model.progress
                |> List.length
    in
    sumRightAnswers >= model.threshold


rightAnswer : Question -> String
rightAnswer quest =
    case quest of
        WhatIsTheSlope slope yIntercept ->
            div [ id "questionText" ]
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "What is the slope?"
                ]

        WhatIsTheIntercept slope yIntercept ->
            div [ id "questionText" ]
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "What is the y-intercept?"
                ]

        WhichGraph slope yIntercept ->
            div [ id "questionText" ]
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "Which graph corresponds to this equation?"
                ]

        WhatIsY slope yIntercept x ->
            div [ id "questionText" ]
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text ("If x = " ++ String.fromInt x ++ "What does y equal?")
                ]


viewButtonPanel : Model -> Html Msg
viewButtonPanel model =
    case model.status of
        WaitingToStart ->
            div [ id "buttonPanel" ]
                [ button
                    [ onClick GetNextQuestion ]
                    [ text "Start" ]
                ]

        WaitingForAnswer ->
            div [ id "buttonPanel" ]
                [ button
                    [ onClick (GotResponse WrongAnswer) ]
                    [ text (String.fromFloat 5) ]
                , button
                    [ onClick (GotResponse WrongAnswer) ]
                    [ text (String.fromFloat (5 + 7)) ]
                , button
                    [ onClick (GotResponse RightAnswer) ]
                    [ text (String.fromFloat 7) ]
                ]

        GotAnswer _ ->
            if crossedThreshold model then
                div [ id "buttonPanel" ]
                    [ button
                        [ onClick ReturnToTorus ]
                        [ text "Return to Torus" ]
                    ]

            else
                div [ id "buttonPanel" ]
                    [ button
                        [ onClick GetNextQuestion ]
                        [ text "Click here to continue" ]
                    ]


boxStyle : List (Attribute msg)
boxStyle =
    [ style "border-radius" "5px"
    , style "padding" "5px"
    , style "width" "10px"
    , style "height" "10px"
    , style "display" "inline-block"
    ]


progressBox : RightOrWrong -> Html Msg
progressBox rOrW =
    if rOrW == RightAnswer then
        div (style "background-color" "green" :: boxStyle) []

    else if rOrW == WrongAnswer then
        div (style "background-color" "red" :: boxStyle) []

    else
        div (style "background-color" "grey" :: boxStyle) []


viewProgressPanel : Model -> Html Msg
viewProgressPanel model =
    if model.status == WaitingToStart then
        div [ id "progressPanel" ] []

    else
        let
            progressBar =
                List.reverse model.progress
                    |> List.map progressBox
        in
        div
            [ id "progressPanel" ]
            progressBar


viewDebugPanel : Model -> Html Msg
viewDebugPanel model =
    if model.debug then
        div [ id "debugPanel" ]
            [ text ("threshold: " ++ String.fromInt model.threshold)
            , text ("window: " ++ String.fromInt model.window)
            ]

    else
        div [ id "debugPanel" ] []


mapNumberToQuestion : Int -> Float -> Float -> Int -> Question
mapNumberToQuestion question slope yIntercept xValue =
    let
        roundSlope =
            Round.roundNum 2 slope

        roundYIntercept =
            Round.roundNum 2 yIntercept
    in
    case question of
        0 ->
            WhatIsTheSlope roundSlope roundYIntercept

        1 ->
            WhatIsTheIntercept roundSlope roundYIntercept

        2 ->
            WhichGraph roundSlope roundYIntercept

        3 ->
            WhatIsY roundSlope roundYIntercept xValue

        _ ->
            WhatIsTheSlope roundSlope roundYIntercept


randomQuestionGenerator : Random.Generator Question
randomQuestionGenerator =
    Random.map4
        mapNumberToQuestion
        (Random.int 0 3)
        (Random.float -10 10)
        (Random.float -10 10)
        (Random.int 0 10)


updateProgress : Int -> List RightOrWrong -> RightOrWrong -> List RightOrWrong
updateProgress window oldRWs newRW =
    let
        newRWs =
            List.append oldRWs [ newRW ]

        countRWs =
            List.length newRWs
    in
    List.drop (countRWs - window) newRWs


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNextQuestion ->
            ( { model | status = WaitingForAnswer }
            , Random.generate GotRandomQuestion randomQuestionGenerator
            )

        GotRandomQuestion newQuestion ->
            ( { model
                | question = newQuestion
                , status = WaitingForAnswer
              }
            , Cmd.none
            )

        GotResponse rightOrWrong ->
            ( { model
                | progress = updateProgress model.window model.progress rightOrWrong
                , status = GotAnswer rightOrWrong
              }
            , Cmd.none
            )

        ReturnToTorus ->
            ( model, sendToTorus True )

        GetDataFromTorus flags ->
            ( { model
                | threshold = flags.threshold
                , window = flags.window
                , debug = flags.debug
                , progress = List.repeat flags.window NothingYet
              }
            , Cmd.none
            )


initialCmd : Cmd Msg
initialCmd =
    Cmd.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , initialCmd
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    getFromTorus GetDataFromTorus


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
