port module LinearEquation exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, id, name, src, title, type_)
import Html.Events exposing (onClick)
import Random
import Round


port sendToTorus : Bool -> Cmd msg


type Question
    = WhatIsTheSlope Float Float
    | WhatIsTheIntercept Float Float
    | WhichGraph Float Float
    | WhatIsY Float Float Int


type RightOrWrong
    = RightAnswer
    | WrongAnswer


type Status
    = WaitingToStart
    | WaitingForAnswer
    | GotAnswer RightOrWrong


type alias Model =
    { question : Question
    , progress : List RightOrWrong
    , status : Status
    , threshold : Int
    , window : Int
    }


initialModel : Model
initialModel =
    { question = WhatIsTheSlope 2 2
    , progress = []
    , status = WaitingToStart
    , threshold = 4
    , window = 5
    }


type Msg
    = GetNextQuestion
    | GotRandomQuestion Question
    | GotResponse RightOrWrong
    | ReturnToTorus


type alias Flags =
    { mastery : Bool
    , threshold : Int
    , window : Int
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Linear Equations" ]
        , text (String.fromInt model.window)
        , viewQuestionPanel model
        , viewFeedbackPanel model
        , viewButtonPanel model
        , viewProgressPanel model
        ]


viewQuestionPanel : Model -> Html Msg
viewQuestionPanel model =
    case model.status of
        WaitingToStart ->
            div [] []

        WaitingForAnswer ->
            viewQuestion model

        GotAnswer _ ->
            div [] []


viewQuestion : Model -> Html Msg
viewQuestion model =
    div []
        [ text "If this is your equation:"
        , questionText model.question
        ]


questionText : Question -> Html Msg
questionText quest =
    case quest of
        WhatIsTheSlope slope yIntercept ->
            div []
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "What is the slope?"
                ]

        WhatIsTheIntercept slope yIntercept ->
            div []
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "What is the y-intercept?"
                ]

        WhichGraph slope yIntercept ->
            div []
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text "Which graph corresponds to this equation?"
                ]

        WhatIsY slope yIntercept x ->
            div []
                [ h3 [] [ text (equationAsString slope yIntercept) ]
                , text ("If x = " ++ String.fromInt x ++ "What does y equal?")
                ]


equationAsString : Float -> Float -> String
equationAsString slope yIntercept =
    if yIntercept < 0 then
        "y = "
            ++ String.fromFloat slope
            ++ "x - "
            ++ String.fromFloat (abs yIntercept)

    else
        "y = "
            ++ String.fromFloat slope
            ++ "x + "
            ++ String.fromFloat yIntercept


viewFeedbackPanel : Model -> Html Msg
viewFeedbackPanel model =
    case model.status of
        WaitingToStart ->
            div [] []

        WaitingForAnswer ->
            div [] [ text "Choose the correct answer" ]

        GotAnswer RightAnswer ->
            div [] [ text "Correct!" ]

        GotAnswer WrongAnswer ->
            div [] [ text "Incorrect" ]


crossedThreshold : Model -> Bool
crossedThreshold model =
    let
        sumRightAnswers =
            List.filter (\p -> p == RightAnswer) model.progress
                |> List.length
    in
    sumRightAnswers >= model.threshold


viewButtonPanel : Model -> Html Msg
viewButtonPanel model =
    case model.status of
        WaitingToStart ->
            div []
                [ button
                    [ onClick GetNextQuestion ]
                    [ text "Start" ]
                ]

        WaitingForAnswer ->
            div []
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
                div []
                    [ button
                        [ onClick ReturnToTorus ]
                        [ text "Return to Torus" ]
                    ]

            else
                div []
                    [ button
                        [ onClick GetNextQuestion ]
                        [ text "Click here to continue" ]
                    ]


viewProgressPanel : Model -> Html Msg
viewProgressPanel model =
    if model.status == WaitingToStart then
        div [] []

    else
        let
            reversedRWs =
                List.reverse model.progress

            plusOrMinus : RightOrWrong -> Html Msg
            plusOrMinus rOrW =
                if rOrW == RightAnswer then
                    text "+"

                else
                    text "-"
        in
        div [] <|
            List.map plusOrMinus reversedRWs


mapNumberToQuestion : Int -> Float -> Float -> Int -> Question
mapNumberToQuestion question slope yIntercept xValue =
    case question of
        0 ->
            WhatIsTheSlope slope yIntercept

        1 ->
            WhatIsTheIntercept slope yIntercept

        2 ->
            WhichGraph slope yIntercept

        3 ->
            WhatIsY slope yIntercept xValue

        _ ->
            WhatIsTheSlope slope yIntercept


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


initialCmd : Cmd Msg
initialCmd =
    Cmd.none


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initialModel | threshold = flags.threshold, window = flags.window }
    , initialCmd
    )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
