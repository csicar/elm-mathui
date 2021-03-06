module Main exposing (..)

import Html exposing (div, span)
import Html.Attributes exposing (..)
import MathUi
import MathUi.Operations exposing (..)


main =
    Html.program { init = ( model, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }


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
        , focus = []
        }
    }


type Msg
    = MathUiMsg MathUi.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MathUiMsg msg ->
            let
                ( newModel, cmds ) =
                    MathUi.update msg model.mathUi
            in
                { model | mathUi = newModel } ! [ Cmd.map MathUiMsg cmds ]


view : Model -> Html.Html Msg
view model =
    div [] [ Html.map MathUiMsg (MathUi.viewAll model.mathUi) ]
