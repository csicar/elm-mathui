Elm-MathUi
==========

Installation
------------

```bash
elm package install csicar/elm-mathui
```

Usage
-----

### minimal Example

[in action](https://csicar.github.io/elm-mathui/example/Example1.html)

```elm
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
                (Hole)
                (Id "a")
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
    div [] [ Html.map MathUiMsg (MathUi.view model.mathUi) ]

```
