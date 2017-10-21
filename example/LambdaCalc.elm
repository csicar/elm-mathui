module Main exposing (..)

import Html exposing (div, span)
import Html.Attributes exposing (..)
import MathUi exposing (sigma, equals, Exp(..), infinity, plus, elemIn, sqrtOp, divide, vectorsymbol, app, lambda)


main =
    Html.program { init = (model, Cmd.none), view = view, update = update, subscriptions = \_ -> Sub.none }


type alias Model =
    { mathUi : MathUi.Model
    }


z =
    lambda (Id "s") (lambda (Id "z") (Id "z"))


succ =
    lambda (Id "a")
        (lambda (Id "b")
            (lambda (Id "c")
                (app (Id "a")
                    (app (Id "b") (app (Id "c") ((Id "a"))))
                )
            )
        )


model : Model
model =
    { mathUi =
        { expression = app (app (Id "b") (Id "a")) (lambda (Id "x") (app (Id "z") (Id "x")))
        , breadCrum = []
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

            { model | mathUi = newModel } ! [Cmd.map (\a -> MathUiMsg a) cmds]


view : Model -> Html.Html Msg
view model =
    div [] [ Html.map MathUiMsg (MathUi.view model.mathUi) ]
