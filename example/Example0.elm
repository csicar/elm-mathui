module Main exposing (..)

import Html exposing (div, span)
import Html.Attributes exposing (..)
import MathUi
import MathUi.Operations exposing (..)


main =
    Html.program { init = (model, Cmd.none), view = view, update = update, subscriptions = \_ -> Sub.none }


type alias Model =
    { mathUi : MathUi.Model
    }


model : Model
model =
    { mathUi =
        { expression =
            plus
                (Hole)
                (Id "a")
        , breadCrum = []
        , focus = []
        }
    }


type Msg
    = MathUiMsg MathUi.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MathUiMsg msg ->
            let
                (newModel, cmds) = MathUi.update msg model.mathUi
            in
              { model | mathUi = newModel } ! [Cmd.map MathUiMsg cmds]

view : Model -> Html.Html Msg
view model =
    div [] [ Html.map MathUiMsg (MathUi.view model.mathUi) ]
