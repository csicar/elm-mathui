module Main exposing (..)

import Html exposing (div, span, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import MathUi
import MathUi.Breadcrums exposing (..)
import MathUi.Operations exposing (..)


main =
    Html.program { init = ( model, Cmd.none ), view = view, update = update, subscriptions = \_ -> Sub.none }


type alias Model =
    { mathUi : MathUi.Model
    }


z =
    lambda (Id "s") (lambda (Id "z") (Id "z"))


aλ : String -> Exp -> Exp
aλ string exp =
    case (string |> String.toList) of
        [] ->
            exp

        x :: xs ->
            lambda (Id (x |> String.fromChar)) (aλ (xs |> String.fromList) exp)


ap : Exp -> String -> Exp
ap func val =
    (app (Id val) (func))


succ =
    lambda (Id "a")
        (lambda (Id "b")
            (lambda (Id "c")
                (app (Id "a")
                    (app (Id "b") (app (Id "c") ((Id "a"))))
                )
            )
        )


churchNumerals =
    { expression = app z succ
    , breadCrum = []
    , focus = []
    }


yCombinator =
    { expression = app (aλ "y" (ap (Id "y") "y")) (aλ "x" (ap (Id "x") "x"))
    , breadCrum = []
    , focus = []
    }


model : Model
model =
    { mathUi = yCombinator
    }


type Msg
    = MathUiMsg MathUi.Msg
    | ChangeModel MathUi.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MathUiMsg msg ->
            let
                ( newModel, cmds ) =
                    MathUi.update msg model.mathUi
            in
                { model | mathUi = newModel } ! [ Cmd.map (\a -> MathUiMsg a) cmds ]

        ChangeModel newModel ->
            { model | mathUi = newModel } ! []


view : Model -> Html.Html Msg
view model =
    div []
        [ Html.map MathUiMsg (MathUi.view model.mathUi)
        , span []
            [ button [ onClick <| ChangeModel churchNumerals ] [ text "ChurchNumerals" ]
            , button [ onClick <| ChangeModel yCombinator ] [ text "YCombinator" ]
            ]
        ]
