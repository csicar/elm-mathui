module Main exposing (..)

import Html exposing (div, span)
import Html.Attributes exposing (..)
import MathUi exposing (sigma, equals, Exp(..), infinity, plus, elemIn, sqrtOp, divide, vectorsymbol)


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { mathUi : MathUi.Model
    }


model : Model
model =
    { mathUi =
        { expression =
            plus
                (sigma
                    (equals (Id "i") (Id "2"))
                    (infinity)
                    (plus
                        (Id "a")
                        (divide (Id "c") (sqrtOp (Id "2")))
                    )
                )
                (elemIn
                    (Vector [ Id "a", Id "b", Id "c" ])
                    (vectorsymbol (Id "R"))
                )
        , breadCrum = []
        }
    }


type Msg
    = MathUiMsg MathUi.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        MathUiMsg msg ->
            { model | mathUi = MathUi.update msg model.mathUi }


view : Model -> Html.Html Msg
view model =
    div [] [ Html.map MathUiMsg (MathUi.viewAll model.mathUi) ]
