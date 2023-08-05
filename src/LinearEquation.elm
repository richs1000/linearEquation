port module LinearEquation exposing (..)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, style, title, type_)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Random


port getFromTorus : (Flags -> msg) -> Sub msg


port sendToTorus : Bool -> Cmd msg


type Answer
    = NumberChoice Int -- when the answers are a single number (e.g., what's the slope?)
    | NoChoice


type alias Choice =
    { answer : Answer
    , feedback : String
    }


emptyChoice : Choice
emptyChoice =
    { answer = NoChoice, feedback = "Empty Feedback" }


type alias RandomOrder =
    { first : Int
    , second : Int
    , third : Int
    }


defaultOrder : RandomOrder
defaultOrder =
    { first = 1, second = 2, third = 3 }


type QuestionType
    = WhatIsTheSlope
    | WhatIsTheIntercept
    | WhatIsY
    | WhichGraph



{-
   To randomize the order of questions, here's what I did:
   * The choices array is always in the same order: right answer, distractor 1, distractor 2
   * The randomOrder record has three fields: first, second and third
       * The first field gives an index from 0 to 2, that index tells you what item in the choices array was displayed in the first button
       * The second field gives an index from 0 to 2, that index tells you what item in the choices array was displayed in the second button
       * The third field gives an index from 0 to 2, that index tells you what item in the choices array was displayed in the third button
-}


type alias Question =
    { questionType : QuestionType
    , slope : Int
    , yIntercept : Int
    , xValue : Int
    , choices : Array.Array Choice -- right choice, wrong choice 1, wrong choice 2
    , randomOrder : RandomOrder -- first button has choice <x>, second button has choice <y>, third button has choice <z>
    }


emptyQuestion : Question
emptyQuestion =
    { questionType = WhatIsTheSlope
    , slope = 0
    , yIntercept = 0
    , xValue = 0
    , choices = Array.empty
    , randomOrder = defaultOrder
    }


type RightOrWrong
    = RightAnswer -- user chose the correct answer
    | WrongAnswer -- user chose the wrong answer
    | NothingYet -- this is used by progress bar when the window is bigger than the number of responses


type Status
    = WaitingToStart -- user needs to ask for next question
    | WaitingForAnswer -- user needs to choose an answer
    | GotAnswer -- user has answered, so now we need to give feedback


type alias Model =
    { question : Question -- What question do we show to the user?
    , userChoice : Int -- Which button did user choose? (1, 2 or 3)
    , progress : List RightOrWrong -- How many questions has the user gotten right/wrong?
    , status : Status -- What should we be showing to the user?
    , threshold : Int -- How many questions does the user need to get right?
    , window : Int -- How big is the window for reaching the threshold?
    , debug : Bool -- Do we show debug information?
    }


initialModel : Model
initialModel =
    { question = emptyQuestion
    , userChoice = 0
    , progress = List.repeat 6 NothingYet
    , status = WaitingToStart
    , threshold = 4
    , window = 6
    , debug = True
    }


type Msg
    = GetNextQuestion -- Generate random numbers for the next question
    | GotRandomQuestion Question -- Use random numbers to create and display next question
    | GotResponse Int -- Give feedback to user about their answer
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
            div [ id "questionPanel" ]
                [ text "If this is your equation:"
                , div [ id "questionText" ]
                    [ h3 [] [ text (equationAsString model.question.slope model.question.yIntercept) ]
                    , text (questionText model.question)
                    ]
                ]

        GotAnswer ->
            div [ id "questionPanel" ] []


questionText : Question -> String
questionText question =
    case question.questionType of
        WhatIsTheSlope ->
            "What is the slope?"

        WhatIsTheIntercept ->
            "What is the y-intercept?"

        WhichGraph ->
            "Which graph corresponds to this equation?"

        WhatIsY ->
            "If x = " ++ String.fromInt question.xValue ++ "what does y equal?"


equationAsString : Int -> Int -> String
equationAsString slope yIntercept =
    let
        operator =
            if yIntercept < 0 then
                "- "

            else
                "+ "
    in
    "y = "
        ++ String.fromInt slope
        ++ "x "
        ++ operator
        ++ String.fromInt (abs yIntercept)


extractFeedback : Model -> String
extractFeedback model =
    let
        index =
            if model.userChoice == model.question.randomOrder.first then
                0

            else if model.userChoice == model.question.randomOrder.second then
                1

            else
                2
    in
    model.question.choices
        |> Array.get index
        |> withDefault emptyChoice
        |> .feedback


viewFeedbackPanel : Model -> Html Msg
viewFeedbackPanel model =
    case model.status of
        WaitingToStart ->
            div [ id "feedbackPanel" ] []

        WaitingForAnswer ->
            div [ id "feedbackPanel" ] [ text "Choose the correct answer" ]

        GotAnswer ->
            div [ id "feedbackPanel" ] [ text (extractFeedback model) ]


crossedThreshold : Model -> Bool
crossedThreshold model =
    let
        sumRightAnswers =
            List.filter (\p -> p == RightAnswer) model.progress
                |> List.length
    in
    sumRightAnswers >= model.threshold


extractAnswer : Int -> Model -> String
extractAnswer buttonIndex model =
    let
        answerIndex =
            if buttonIndex == model.question.randomOrder.first then
                0

            else if buttonIndex == model.question.randomOrder.second then
                1

            else
                2

        answer =
            model.question.choices
                |> Array.get answerIndex
                |> withDefault emptyChoice
                |> .answer
    in
    case answer of
        NoChoice ->
            "No Choice"

        NumberChoice numberAnswer ->
            String.fromInt numberAnswer


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
                    [ onClick (GotResponse 0) ]
                    [ text (extractAnswer 0 model) ]
                , button
                    [ onClick (GotResponse 1) ]
                    [ text (extractAnswer 1 model) ]
                , button
                    [ onClick (GotResponse 2) ]
                    [ text (extractAnswer 2 model) ]
                ]

        GotAnswer ->
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


getRandomOrder : Int -> RandomOrder
getRandomOrder randomOrder =
    case randomOrder of
        0 ->
            { first = 0, second = 1, third = 2 }

        1 ->
            { first = 0, second = 2, third = 1 }

        2 ->
            { first = 1, second = 0, third = 2 }

        3 ->
            { first = 1, second = 2, third = 0 }

        4 ->
            { first = 2, second = 0, third = 1 }

        _ ->
            { first = 2, second = 1, third = 0 }


uniqueFrom : Int -> Int -> Int -> ( Int, Int )
uniqueFrom base x y =
    let
        xPrime =
            if base == x then
                x + 1

            else
                x

        yPrime =
            if base == y && (y - 1) /= xPrime then
                y - 1

            else if base == y then
                y - 2

            else
                y
    in
    ( xPrime, yPrime )


whatIsTheSlopeQuestion : Int -> Int -> Int -> Question
whatIsTheSlopeQuestion slope yIntercept randomOrder =
    let
        ( yInterceptUnique, distractor2 ) =
            uniqueFrom slope yIntercept (slope + yIntercept)

        choices =
            Array.fromList
                [ { answer = NumberChoice slope, feedback = "Correct!" }
                , { answer = NumberChoice yInterceptUnique, feedback = "dist 1" }
                , { answer = NumberChoice distractor2, feedback = "dist 2" }
                ]
    in
    { questionType = WhatIsTheSlope
    , slope = slope
    , yIntercept = yInterceptUnique
    , xValue = 0
    , choices = choices
    , randomOrder = getRandomOrder randomOrder
    }



{-
   type alias Question =
       { questionType : QuestionType
       , slope : Int
       , yIntercept : Int
       , xValue : Int
       , choices : Array.Array Choice -- right choice, wrong choice 1, wrong choice 2
       , randomOrder : RandomOrder -- first button has choice <x>, second button has choice <y>, third button has choice <z>
       }
-}


makeQuestion : Int -> Int -> Int -> Int -> Int -> Question
makeQuestion whatQuestion slope yIntercept xValue randomOrder =
    case whatQuestion of
        0 ->
            whatIsTheSlopeQuestion slope yIntercept randomOrder

        _ ->
            whatIsTheSlopeQuestion slope yIntercept randomOrder



-- 1 ->
--     WhatIsTheIntercept roundSlope roundYIntercept
-- 2 ->
--     WhichGraph roundSlope roundYIntercept
-- 3 ->
--     WhatIsY roundSlope roundYIntercept xValue
-- _ ->
--     WhatIsTheSlope roundSlope roundYIntercept


randomQuestionGenerator : Random.Generator Question
randomQuestionGenerator =
    Random.map5
        makeQuestion
        (Random.int 0 3)
        (Random.int -10 10)
        (Random.int -10 10)
        (Random.int 0 10)
        (Random.int 0 5)


updateProgress : Int -> List RightOrWrong -> Int -> Int -> List RightOrWrong
updateProgress window oldRWs rightAnswer userAnswer =
    let
        newRW =
            if rightAnswer == userAnswer then
                RightAnswer

            else
                WrongAnswer

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

        GotResponse userChoice ->
            ( { model
                | progress = updateProgress model.window model.progress model.question.randomOrder.first userChoice
                , status = GotAnswer
                , userChoice = userChoice
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
