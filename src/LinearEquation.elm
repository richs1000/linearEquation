{-

   To Do List:
   + Fix styling
   + Fix feedback
   + Make size of each panel consistent
   * Add in graph questions
-}


port module LinearEquation exposing (..)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, style, title, type_)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Random
import Svg
import Svg.Attributes


port getFromTorus : (Flags -> msg) -> Sub msg


port sendToTorus : Bool -> Cmd msg


type Answer
    = NumberChoice Int -- when the answers are a single number (e.g., what's the slope?)
    | LineGraphChoice String Int Int -- when the answer is a graph of an equation (name, slope, y-intercept)
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


questionPanelStyle : List (Attribute msg)
questionPanelStyle =
    [ style "padding" "5px"
    , style "width" "800px"
    , style "height" "300px"
    ]


viewQuestionPanel : Model -> Html Msg
viewQuestionPanel model =
    case model.status of
        WaitingToStart ->
            div (id "questionPanel" :: questionPanelStyle)
                []

        WaitingForAnswer ->
            div (id "questionPanel" :: questionPanelStyle)
                [ text "If this is your equation:"
                , div [ id "questionText" ]
                    [ h3 [] [ text (equationAsString model.question.slope model.question.yIntercept) ]
                    , questionText model
                    ]
                ]

        GotAnswer ->
            div (id "questionPanel" :: questionPanelStyle)
                []


questionText : Model -> Html Msg
questionText model =
    case model.question.questionType of
        WhatIsTheSlope ->
            text "What is the slope?"

        WhatIsTheIntercept ->
            text "What is the y-intercept?"

        WhichGraph ->
            whichGraphQuestion model

        WhatIsY ->
            text ("If x = " ++ String.fromInt model.question.xValue ++ " what does y equal?")


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


getIndex : RandomOrder -> Int -> Int
getIndex randomOrder x =
    if x == randomOrder.first then
        0

    else if x == randomOrder.second then
        1

    else
        2


extractFeedback : Model -> String
extractFeedback model =
    let
        index =
            getIndex model.question.randomOrder model.userChoice
    in
    model.question.choices
        |> Array.get index
        |> withDefault emptyChoice
        |> .feedback


feedbackPanelStyle : List (Attribute msg)
feedbackPanelStyle =
    [ style "padding" "5px"
    , style "width" "600px"
    , style "height" "50px"
    ]


viewFeedbackPanel : Model -> Html Msg
viewFeedbackPanel model =
    case model.status of
        WaitingToStart ->
            div (id "feedbackPanel" :: feedbackPanelStyle)
                []

        WaitingForAnswer ->
            div (id "feedbackPanel" :: feedbackPanelStyle)
                []

        GotAnswer ->
            div (id "feedbackPanel" :: feedbackPanelStyle)
                [ text (extractFeedback model) ]


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
            getIndex model.question.randomOrder buttonIndex

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

        LineGraphChoice graphName _ _ ->
            String.fromChar (Char.fromCode (65 + buttonIndex))


buttonPanelStyle : List (Attribute msg)
buttonPanelStyle =
    [ style "border-radius" "5px"
    , style "padding" "5px"
    , style "width" "600px"
    , style "height" "50px"
    ]


answerButtonStyle : List (Attribute msg)
answerButtonStyle =
    [ style "border-radius" "5px"
    , style "padding" "5px"
    , style "width" "40px"
    , style "height" "40px"
    , style "display" "inline"
    ]


viewButtonPanel : Model -> Html Msg
viewButtonPanel model =
    case model.status of
        WaitingToStart ->
            div (id "buttonPanel" :: buttonPanelStyle)
                [ button
                    [ onClick GetNextQuestion ]
                    [ text "Start" ]
                ]

        WaitingForAnswer ->
            div (id "buttonPanel" :: buttonPanelStyle)
                [ button
                    (onClick (GotResponse 0) :: answerButtonStyle)
                    [ text (extractAnswer 0 model) ]
                , button
                    (onClick (GotResponse 1) :: answerButtonStyle)
                    [ text (extractAnswer 1 model) ]
                , button
                    (onClick (GotResponse 2) :: answerButtonStyle)
                    [ text (extractAnswer 2 model) ]
                ]

        GotAnswer ->
            if crossedThreshold model then
                div (id "buttonPanel" :: buttonPanelStyle)
                    [ button
                        [ onClick ReturnToTorus ]
                        [ text "Return to Torus" ]
                    ]

            else
                div (id "buttonPanel" :: buttonPanelStyle)
                    [ button
                        [ onClick GetNextQuestion ]
                        [ text "Click here to continue" ]
                    ]


progressPanelStyle : List (Attribute msg)
progressPanelStyle =
    [ style "border-radius" "5px"
    , style "padding" "5px"
    , style "width" "600px"
    , style "height" "50px"
    ]


progressBarStyle : List (Attribute msg)
progressBarStyle =
    [ style "border-radius" "5px"
    , style "padding" "5px"
    , style "width" "10px"
    , style "height" "10px"
    , style "display" "inline-block"
    ]


progressBox : RightOrWrong -> Html Msg
progressBox rOrW =
    if rOrW == RightAnswer then
        div (style "background-color" "green" :: progressBarStyle) []

    else if rOrW == WrongAnswer then
        div (style "background-color" "red" :: progressBarStyle) []

    else
        div (style "background-color" "grey" :: progressBarStyle) []


viewProgressPanel : Model -> Html Msg
viewProgressPanel model =
    if model.status == WaitingToStart then
        div (id "progressPanel" :: progressPanelStyle)
            []

    else
        let
            progressBar =
                List.reverse model.progress
                    |> List.map progressBox
        in
        div
            (id "progressPanel" :: progressPanelStyle)
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


whatIsTheInterceptQuestion : Int -> Int -> Int -> Int -> Question
whatIsTheInterceptQuestion slope yIntercept xValue randomOrder =
    let
        choices =
            Array.fromList
                [ { answer = NumberChoice yIntercept, feedback = "Correct! The y-intercept is " ++ String.fromInt yIntercept }
                , { answer = NumberChoice slope, feedback = "That's the slope. The y-intercept is " ++ String.fromInt yIntercept }
                , { answer = NumberChoice xValue, feedback = "That is incorrect. The y-intercept is " ++ String.fromInt yIntercept }
                ]
    in
    { questionType = WhatIsTheIntercept
    , slope = slope
    , yIntercept = yIntercept
    , xValue = xValue
    , choices = choices
    , randomOrder = getRandomOrder randomOrder
    }


whatIsTheSlopeQuestion : Int -> Int -> Int -> Int -> Question
whatIsTheSlopeQuestion slope yIntercept xValue randomOrder =
    let
        choices =
            Array.fromList
                [ { answer = NumberChoice slope, feedback = "Correct! The slope is " ++ String.fromInt slope }
                , { answer = NumberChoice yIntercept, feedback = "That's the y-intercept. The slope is " ++ String.fromInt slope }
                , { answer = NumberChoice xValue, feedback = "That is incorrct. The slope is " ++ String.fromInt slope }
                ]
    in
    { questionType = WhatIsTheSlope
    , slope = slope
    , yIntercept = yIntercept
    , xValue = xValue
    , choices = choices
    , randomOrder = getRandomOrder randomOrder
    }


whatIsYQuestion : Int -> Int -> Int -> Int -> Question
whatIsYQuestion slope yIntercept xValue randomOrder =
    let
        yValue =
            slope * xValue + yIntercept

        choices =
            Array.fromList
                [ { answer = NumberChoice yValue, feedback = "Correct! The value of y is " ++ String.fromInt yValue }
                , { answer = NumberChoice yIntercept, feedback = "That is the y-intercept. The value of y is " ++ String.fromInt yValue }
                , { answer = NumberChoice (yIntercept * xValue + slope), feedback = "That is incorrect. The value of y is " ++ String.fromInt yValue }
                ]
    in
    { questionType = WhatIsY
    , slope = slope
    , yIntercept = yIntercept
    , xValue = xValue
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


uniqueValues : Int -> Int -> Int -> ( Int, Int, Int )
uniqueValues slope yIntercept xValue =
    -- get yValue
    let
        yValue =
            slope * xValue + yIntercept

        yValueWrong =
            yIntercept * xValue + slope
    in
    if slope == 20 || yIntercept == -20 then
        ( slope, yIntercept, xValue )
        -- if slope = yIntercept then change yIntercept

    else if slope == 0 || slope == yIntercept || slope == yValue || slope == xValue || slope == yValueWrong then
        uniqueValues (slope + 1) yIntercept xValue
        -- if slope = yValue then change yIntercept

    else if yIntercept == 0 || yIntercept == xValue || yIntercept == yValue || yIntercept == yValueWrong then
        uniqueValues slope (yIntercept - 1) xValue
        -- if yIntercept = yValue then change slope

    else if xValue == yValue || xValue == yValueWrong then
        uniqueValues (slope + 1) yIntercept xValue

    else if yValue == yValueWrong then
        uniqueValues (slope + 1) yIntercept xValue

    else
        ( slope, yIntercept, xValue )


makeQuestion : Int -> Int -> Int -> Int -> Int -> Question
makeQuestion whatQuestion slope yIntercept xValue randomOrder =
    let
        ( slopeUnique, yInterceptUnique, xValueUnique ) =
            uniqueValues slope yIntercept xValue
    in
    case whatQuestion of
        0 ->
            whatIsTheSlopeQuestion slopeUnique yInterceptUnique xValueUnique randomOrder

        1 ->
            whatIsTheInterceptQuestion slopeUnique yInterceptUnique xValueUnique randomOrder

        2 ->
            whatIsYQuestion slopeUnique yInterceptUnique xValueUnique randomOrder

        3 ->
            whatIsGraphQuestion slopeUnique yInterceptUnique xValueUnique randomOrder

        _ ->
            whatIsTheSlopeQuestion slopeUnique yInterceptUnique xValueUnique randomOrder



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
        -- what kind of question
        (Random.int 0 3)
        -- random slope
        (Random.int -3 3)
        -- random yIntercept
        (Random.int -8 8)
        -- random xValue
        (Random.int 1 10)
        -- random order to display choices
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


canvasHeight : Int
canvasHeight =
    200


canvasWidth : Int
canvasWidth =
    200


maxX : Int
maxX =
    20


minX : Int
minX =
    0


maxY : Int
maxY =
    50


minY : Int
minY =
    -50


intervalX : Int
intervalX =
    (canvasWidth - (2 * graphPadding)) // (maxX - minX)


intervalY : Int
intervalY =
    (canvasHeight - (2 * graphPadding)) // (maxY - minY)


graphPadding : Int
graphPadding =
    5


graphOrigin : ( Int, Int )
graphOrigin =
    ( graphPadding
    , canvasHeight // 2
    )


graphOriginString : ( String, String )
graphOriginString =
    ( String.fromInt (Tuple.first graphOrigin)
    , String.fromInt (Tuple.second graphOrigin)
    )


toCanvasCoordinates : ( Int, Int ) -> ( Int, Int )
toCanvasCoordinates ( x, y ) =
    ( toCanvasX x
    , toCanvasY y
    )


toCanvasX : Int -> Int
toCanvasX x =
    Tuple.first graphOrigin + (x * intervalX)


toCanvasY : Int -> Int
toCanvasY y =
    Tuple.second graphOrigin - (y * intervalY)


toCanvasXString : Int -> String
toCanvasXString x =
    toCanvasX x
        |> String.fromInt


toCanvasYString : Int -> String
toCanvasYString y =
    toCanvasY y
        |> String.fromInt


whatIsGraphQuestion : Int -> Int -> Int -> Int -> Question
whatIsGraphQuestion slope yIntercept xValue randomOrder =
    let
        choices =
            Array.fromList
                [ { answer = LineGraphChoice "Correct" slope yIntercept, feedback = "Correct!" }
                , { answer = LineGraphChoice "wrong slope" (slope * -1) yIntercept, feedback = "That is the wrong slope" }
                , { answer = LineGraphChoice "wrong y-intercept" slope (yIntercept * -1), feedback = "That is the wrong y-intercept" }
                ]
    in
    { questionType = WhichGraph
    , slope = slope
    , yIntercept = yIntercept
    , xValue = xValue
    , choices = choices
    , randomOrder = getRandomOrder randomOrder
    }


whichGraphQuestion : Model -> Html Msg
whichGraphQuestion model =
    div [ id "graphQuestion" ]
        [ text "Which graph corresponds to this equation?"
        , div [ id "graphPanel" ]
            [ drawGraph 0 model "A"
            , drawGraph 1 model "B"
            , drawGraph 2 model "C"
            ]
        ]



-- extractAnswer : Int -> Model -> String
-- extractAnswer buttonIndex model =


drawGraph : Int -> Model -> String -> Html msg
drawGraph graphIndex model name =
    let
        answerIndex =
            getIndex model.question.randomOrder graphIndex

        answer =
            model.question.choices
                |> Array.get answerIndex
                |> withDefault emptyChoice
                |> .answer
    in
    div [ style "display" "inline-block" ]
        [ div [ style "display" "block" ]
            [ Svg.svg
                [ Svg.Attributes.width (String.fromInt canvasWidth)
                , Svg.Attributes.height (String.fromInt canvasHeight)

                --, Svg.Attributes.viewBox "0 0 200 200"
                , Html.Attributes.style "border-width" "2px"
                , Html.Attributes.style "border-style" "solid"
                , Html.Attributes.style "border-color" "black"
                ]
                (drawLine answer :: drawAxes)
            ]
        , div
            [ style "display" "block"
            , style "text-align" "center"
            ]
            [ text name ]
        ]


drawAxes : List (Svg.Svg msg)
drawAxes =
    [ Svg.line
        [ Svg.Attributes.x1 (toCanvasXString 0)
        , Svg.Attributes.y1 (String.fromInt graphPadding)
        , Svg.Attributes.x2 (toCanvasXString 0)
        , Svg.Attributes.y2 (String.fromInt (canvasHeight - graphPadding))
        , Svg.Attributes.stroke "black"
        ]
        []
    , Svg.line
        [ Svg.Attributes.x1 (toCanvasXString 0)
        , Svg.Attributes.y1 (toCanvasYString 0)
        , Svg.Attributes.x2 (toCanvasXString maxX)
        , Svg.Attributes.y2 (toCanvasYString 0)
        , Svg.Attributes.stroke "black"
        ]
        []
    ]



-- type Answer
--     = NumberChoice Int -- when the answers are a single number (e.g., what's the slope?)
--     | LineGraphChoice String Int Int -- when the answer is a graph of an equation (name, slope, y-intercept)
--     | NoChoice
-- type alias Choice =
--     { answer : Answer
--     , feedback : String
--     }


drawLine : Answer -> Svg.Svg msg
drawLine answer =
    case answer of
        LineGraphChoice _ slope yIntercept ->
            let
                x1 =
                    0

                x2 =
                    maxX

                y1 =
                    slope * x1 + yIntercept

                y2 =
                    slope * x2 + yIntercept
            in
            Svg.line
                [ Svg.Attributes.x1 (toCanvasXString x1)
                , Svg.Attributes.y1 (toCanvasYString y1)
                , Svg.Attributes.x2 (toCanvasXString x2)
                , Svg.Attributes.y2 (toCanvasYString y2)
                , Svg.Attributes.stroke "black"
                ]
                []

        _ ->
            Svg.line
                [ Svg.Attributes.x1 (toCanvasXString 0)
                , Svg.Attributes.y1 (toCanvasYString 0)
                , Svg.Attributes.x2 (toCanvasXString 0)
                , Svg.Attributes.y2 (toCanvasYString 0)
                , Svg.Attributes.stroke "black"
                ]
                []
