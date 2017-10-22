module Main exposing (..)

import Html exposing (div, span, button, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MathUi
import MathUi.Operations exposing (..)
import MathUi.Breadcrums exposing (..)
import MathUi.Evaluators exposing (..)


main =
    Html.program { init = (model, Cmd.none), view = view, update = update, subscriptions = \_ -> Sub.none }


type alias Model =
    { mathUi : MathUi.Model,
    value : Result (BreadCrum, String) Float
    }


model : Model
model =
    { mathUi =
        { expression =
            sigma (Id "3") (equals (Id "i") (Id "1")) (Id "i")
        , breadCrum = []

        },
      value = Result.Err ([], "no value")
    }


type Msg
    = MathUiMsg MathUi.Msg
    | Evaluate


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MathUiMsg msg ->
            let
                (newModel, cmds) = MathUi.update msg model.mathUi
            in
              { model | mathUi = newModel, value =  numMath model.mathUi.expression [] } ! [Cmd.map MathUiMsg cmds]
        Evaluate ->
            {model | value = numMath model.mathUi.expression []} ! []

viewResult : Result a b -> Html.Html Msg
viewResult baResult = text <| toString baResult


view : Model -> Html.Html Msg
view model =
    div []
      [ Html.map MathUiMsg (MathUi.view model.mathUi)
      , div []
        [ button [onClick Evaluate] [text "evaluate"]
        , viewResult model.value
        ]

      ]
