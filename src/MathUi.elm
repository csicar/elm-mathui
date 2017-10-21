module MathUi exposing (OpInfo, Exp(..), Model, Msg, update, view, viewAll, latexRepr, plus, divide, pow, elemIn, equals, sqrtOp, parentheses, sigma, infinity, vectorsymbol, app, lambda)

{-| MathUi


# Types

@docs OpInfo, Exp, Model, Msg


# Update

@docs update, view, viewAll, latexRepr


# Configuration


##


## build-in constructs

@docs plus, divide, pow, elemIn, equals, sqrtOp, parentheses, sigma, infinity, vectorsymbol, app, lambda

-}

import Html exposing (Html, Attribute, div, input, text, span, node, pre, img, table, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode, onClick, onWithOptions)
import Json.Decode as Json
import Css.File exposing (CssFileStructure, CssCompilerProgram)
import MathUiCss
import Dom exposing (focus)
import Task
import Bitwise exposing (shiftLeftBy)
import Char


main =
    Html.program { init = (model, Cmd.none), view = view, update = update, subscriptions = \x -> Sub.none }


type LatexOperator
    = Operator String
    | AdvancedOperator String


{-| Contains Information for the Operation.
-}
type alias OpInfo =
    { shortName : String
    , longName : String
    , cssClass : String
    , latexOperator : String
    , latexBefore : String
    , latexAfter : String
    }


type OpType
    = App
    | Abs
    | Plus
    | Mul
    | Div
    | Pow
    | Sub
    | Contains
    | Equals
    | FunctionApplication
    | Infinity
    | Root Int
    | NoOp
    | BigSum


{-| Homogeneous syntax tree
-}
type Exp
    = BinOp OpType OpInfo Exp Exp
    | BigOp OpType OpInfo Exp Exp Exp
    | UnaryOp OpType OpInfo Exp
    | Symbol OpType OpInfo
    | Vector (List Exp)
    | Matrix (List (List Exp))
    | Id String
    | Hole


plusInfo =
    { shortName = "+", longName = "plus", cssClass = "Plus", latexOperator = "+", latexBefore = "", latexAfter = "" }


{-| plus left right
-}
plus : Exp -> Exp -> Exp
plus =
    BinOp Plus plusInfo


minus : Exp -> Exp -> Exp
minus =
    BinOp Sub { shortName = "-", longName = "subtract", cssClass = "-", latexOperator = "-", latexBefore = "", latexAfter = "" }


{-| multiply left right
-}
multiply : Exp -> Exp -> Exp
multiply =
    BinOp Mul { shortName = "*", longName = "multiply", cssClass = "Multiply", latexOperator = "\\cdot", latexBefore = "", latexAfter = "" }


{-| divide top bottom
-}
divide : Exp -> Exp -> Exp
divide =
    BinOp Div { shortName = "/", longName = "divide", cssClass = "Div", latexOperator = "\\over", latexBefore = "", latexAfter = "" }


{-| pow basis exponent
-}
pow : Exp -> Exp -> Exp
pow =
    BinOp Pow { shortName = "^", longName = "power", cssClass = "Pow", latexOperator = "^", latexBefore = "", latexAfter = "" }


sub : Exp -> Exp -> Exp
sub =
    BinOp Sub { shortName = "_", longName = "subscript", cssClass = "Sub", latexOperator = "_", latexBefore = "", latexAfter = "" }


{-| elemIn set item
-}
elemIn : Exp -> Exp -> Exp
elemIn =
    BinOp Contains { shortName = "∈", longName = "in", cssClass = "ElemIn", latexOperator = "\\in", latexBefore = "", latexAfter = "" }


{-| equals left right
-}
equals : Exp -> Exp -> Exp
equals =
    BinOp Equals { shortName = "=", longName = "equals", cssClass = "Equals", latexOperator = "=", latexBefore = "", latexAfter = "" }


{-| functionApplication func argument
-}
functionApplication : Exp -> Exp -> Exp
functionApplication =
    BinOp FunctionApplication { shortName = "⇒", longName = "functionApplication", cssClass = "FunctionApplication", latexOperator = "", latexBefore = "", latexAfter = "" }


lambdaInfo : OpInfo
lambdaInfo =
    { shortName = "λ", longName = "lambda", cssClass = "Lambda", latexOperator = "", latexBefore = "", latexAfter = "" }


{-| Represents a lambda abstraction
lambda param body
-}
lambda : Exp -> Exp -> Exp
lambda =
    BinOp Abs lambdaInfo


appInfo =
    { shortName = "β", longName = "app", cssClass = "App", latexOperator = "", latexBefore = "", latexAfter = "" }


{-| Represents a lambda app. Ware attention! the order is flipped compared to standart lambda-calc notation
app argument body
-}
app : Exp -> Exp -> Exp
app =
    BinOp App appInfo


{-| sqrtOp operand
-}
sqrtOp : Exp -> Exp
sqrtOp =
    UnaryOp (Root 2) { shortName = "√", longName = "sqrt", cssClass = "Sqrt", latexOperator = "\\sqrt", latexBefore = "", latexAfter = "" }


{-| enclose expression in parentheses
-}
parentheses : Exp -> Exp
parentheses =
    UnaryOp NoOp { shortName = "()", longName = "parentheses", cssClass = "Parentheses", latexOperator = "()", latexBefore = "(", latexAfter = ")" }


{-| place vector arrow above expression
-}
vectorsymbol : Exp -> Exp
vectorsymbol =
    UnaryOp NoOp { shortName = "vec", longName = "→", cssClass = "VectorSymbol", latexOperator = "\\vec", latexBefore = "", latexAfter = "" }


{-| sigma from to over
-}
sigma : Exp -> Exp -> Exp -> Exp
sigma =
    BigOp BigSum { shortName = "Σ", longName = "sigma", cssClass = "Sigma", latexOperator = "\\sum", latexBefore = "_", latexAfter = "^" }


{-| infinity symbol
-}
infinity : Exp
infinity =
    Symbol Infinity { shortName = "∞", longName = "infinity", cssClass = "Infinity", latexOperator = "\\infty", latexBefore = "", latexAfter = "" }


options =
    [ ( "+", \rest -> plus (Id rest) Hole )
    , ( "-", \rest -> minus (Id rest) Hole )
    , ( "*", \rest -> multiply (Id rest) Hole )
    , ( "/", \rest -> divide (Id rest) Hole )
    , ( "^", \rest -> pow (Id rest) Hole )
    , ( "_", \rest -> sub (Id rest) Hole )
    , ( "=", \rest -> equals (Id rest) Hole )
    , ( "\\lambda", \rest -> lambda (Id rest) (Hole) )
    , ( "\\app", \rest -> app (Id rest) (Hole) )
    , ( "\\in", \rest -> elemIn (Id rest) Hole )
    , ( "\\sqrt", \rest -> sqrtOp (Id rest) )
    , ( "\\vec", \rest -> vectorsymbol (Id rest) )
    , ( "("
      , \rest ->
            case rest of
                "" ->
                    parentheses (Hole)

                rest ->
                    functionApplication (Id rest) Hole
      )
    , ( "\\infinity", \_ -> infinity )
    , ( "\\sigma", \rest -> sigma Hole Hole (Id rest) )
    , ( "\\v2", \rest -> Vector [ Id rest, Hole ] )
    , ( "\\v3", \rest -> Vector [ Id rest, Hole, Hole ] )
    , ( "\\vv"
      , \rest ->
            case (String.toInt rest) of
                Ok value ->
                    Vector <| List.repeat value Hole

                Err _ ->
                    Vector [ Id rest, Hole ]
      )
    , ( "\\m3", \rest -> List.repeat 3 () |> List.map (\a -> List.repeat 3 Hole) |> Matrix )
    , ( "\\mm"
      , \rest ->
            String.split "x" rest
                |> List.map String.toInt
                |> \res ->
                    case res of
                        [ Ok x, Ok y ] ->
                            Matrix <| List.repeat x (List.repeat y Hole)

                        _ ->
                            Matrix [ [ Hole ] ]
      )
    ]


{-| Represents the Expression as a latex-readable string.

    latexRepr plus (Id "a") (Id "b") -- = {{{a}}+{{b}}}

-}
latexRepr : Exp -> String
latexRepr exp =
    case exp of
        BinOp _ info a b ->
            String.concat [ "{", info.latexBefore, "{", latexRepr a, "}", info.latexOperator, "{", latexRepr b, "}", info.latexAfter, "}" ]

        BigOp _ info under over exp ->
            String.concat [ "{", info.latexOperator, info.latexBefore, "{", latexRepr under, "}", info.latexAfter, "{", latexRepr over, "}", "{", latexRepr exp, "}}" ]

        UnaryOp _ info exp ->
            String.concat [ "{", info.latexBefore, info.latexOperator, "{", latexRepr exp, "}", info.latexAfter, "}" ]

        Symbol _ info ->
            String.concat [ "{", info.latexBefore, info.latexOperator, info.latexAfter, "}" ]

        Vector exps ->
            String.concat <| [ "\\begin{bmatrix} " ] ++ List.map (\val -> (latexRepr val) ++ " \\\\ ") exps ++ [ " \\end{bmatrix}" ]

        Matrix rows ->
            let
                reprRow row =
                    List.map latexRepr row |> String.join " & "
            in
                String.concat <| [ "\\begin{pmatrix} ", String.join "\\\\" (List.map reprRow rows), "\\end{pmatrix}" ]

        Id string ->
            String.concat [ "{", string, "}" ]

        Hole ->
            "<Hole>"



--
-- asciiMathRepr : Exp -> String
-- asciiMathRepr exp =
--   case exp of
--     BinOp info a b ->
--       String.concat ["((", asciiMathRepr a, ")", info.shortName, "(", asciiMathRepr, "))"]
--     BigOp info under over exp ->
--       String.concat ["((", info.asciiMathRepr under]


type Crum
    = BinOpLeft OpType OpInfo Exp
    | BinOpRight OpType OpInfo Exp
    | BigOpUnder OpType OpInfo Exp Exp
    | BigOpOver OpType OpInfo Exp Exp
    | BigOpAfer OpType OpInfo Exp Exp
    | UnaryOpHere OpType OpInfo Exp
    | SymbolHere OpType OpInfo
    | VectorAt Int (List Exp) (List Exp)
    | MatrixAt ( Int, Int ) (List (List Exp)) (List Exp) (List Exp) (List (List Exp))
    | IdHere String
    | ExpBelow Exp
    | HoleHere


type alias BreadCrum =
    List Crum


{-| -}
type alias Model =
    { expression : Exp
    , breadCrum : BreadCrum
    }


model : Model
model =
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
                (Id "R")
            )
    , breadCrum = []
    }


model2 =
    { expression = Hole
    , breadCrum = []
    }



-- UPDATE


{-| -}
type Msg
    = UpdateIdentifier BreadCrum String
    | KeyUp BreadCrum Int
    | ClickOn BreadCrum
    | FocusOn String
    | FocusResult (Result Dom.Error ())


textToExp : String -> Exp
textToExp string =
    let
        matchingOptions =
            List.filter (\( marker, constructor ) -> String.endsWith marker string) options
    in
        case List.head matchingOptions of
            Just ( marker, constructor ) ->
                constructor <| String.left ((String.length string) - (String.length marker)) string

            Nothing ->
                Id string


foldCrum : (Crum -> Maybe Exp) -> Exp -> BreadCrum -> Exp
foldCrum f neutral breadCrum =
    case breadCrum of
        x :: [] ->
            f x
                |> Debug.log "f x"
                |> Maybe.withDefault
                    (case x of
                        SymbolHere op opInfo ->
                            Symbol op opInfo

                        ExpBelow exp ->
                            exp

                        IdHere s ->
                            Id s

                        HoleHere ->
                            Hole

                        _ ->
                            neutral
                    )

        x :: xs ->
            let
                futureExp =
                    foldCrum f neutral xs |> Debug.log "exp"
            in
                case x of
                    BinOpLeft op opInfo exp ->
                        BinOp op opInfo futureExp exp

                    BinOpRight op opInfo exp ->
                        BinOp op opInfo exp futureExp

                    BigOpUnder op opInfo over after ->
                        BigOp op opInfo futureExp over after

                    BigOpOver op opInfo under after ->
                        BigOp op opInfo under futureExp after

                    BigOpAfer op opInfo under over ->
                        BigOp op opInfo under over futureExp

                    UnaryOpHere op opInfo inner ->
                        UnaryOp op opInfo futureExp

                    VectorAt pos before after ->
                        Vector (before ++ [ futureExp ] ++ after)

                    MatrixAt pos rowsBefore cellsBefore cellsAfter rowsAfter ->
                        rowsBefore ++ [ cellsBefore ++ [ futureExp ] ++ cellsAfter ] ++ rowsAfter |> Matrix

                    SymbolHere op opInfo ->
                        Symbol op opInfo

                    ExpBelow exp ->
                        exp

                    IdHere s ->
                        Id s

                    HoleHere ->
                        Hole

        [] ->
            neutral


changeIdentifierFolder : String -> Crum -> Maybe Exp
changeIdentifierFolder newContent crum =
    let
        newExp =
            textToExp newContent |> Just
    in
        case crum of
            IdHere string ->
                newExp

            SymbolHere _ _ ->
                newExp

            HoleHere ->
                newExp

            _ ->
                Nothing


changeIdentifier : BreadCrum -> String -> Exp
changeIdentifier breadCrum newContent =
    foldCrum (changeIdentifierFolder newContent) Hole breadCrum


deletePartWithOption : BreadCrum -> Crum -> Exp -> ( Exp, Exp )
deletePartWithOption breadCrum crum changedExp =
    case crum of
        BinOpLeft op opInfo exp ->
            ( BinOp op opInfo changedExp exp, exp )

        BinOpRight op opInfo exp ->
            ( BinOp op opInfo exp changedExp, exp )

        BigOpUnder op opInfo over after ->
            ( BigOp op opInfo changedExp over after, after )

        BigOpOver op opInfo under after ->
            ( BigOp op opInfo under changedExp after, after )

        BigOpAfer op opInfo under over ->
            ( BigOp op opInfo under over changedExp, Hole )

        UnaryOpHere op opInfo _ ->
            ( UnaryOp op opInfo changedExp, Hole )

        VectorAt pos before after ->
            ( Vector (before ++ [ changedExp ] ++ after), Vector (before ++ after) )

        MatrixAt pos rowsBefore cellsBefore cellsAfter rowsAfter ->
            ( rowsBefore ++ [ cellsBefore ++ [ changedExp ] ++ cellsAfter ] ++ rowsAfter |> Matrix, rowsBefore ++ rowsAfter |> Matrix )

        SymbolHere op opInfo ->
            ( Hole, Hole )

        IdHere "" ->
            ( Hole, Hole )

        IdHere s ->
            ( Id s, Id s )

        ExpBelow exp ->
            ( exp, Hole )

        HoleHere ->
            ( Hole, Hole )


deleteNode : BreadCrum -> Exp
deleteNode breadCrum =
    case breadCrum of
        x :: xs ->
            let
                changedExp =
                    deleteNode xs

                ( partDeleted, nodeDeleted ) =
                    deletePartWithOption xs x changedExp
            in
                case xs of
                    [ HoleHere ] ->
                        nodeDeleted

                    _ ->
                        partDeleted

        [] ->
            Hole


applyBetaReduce : Exp -> Exp -> Exp -> Exp
applyBetaReduce argument param body =
    if body == param then
        argument |> Debug.log "arg"
    else
        case body of
            BinOp op opInfo exp exp2 ->
                BinOp op opInfo (applyBetaReduce argument param exp) (applyBetaReduce argument param exp2)

            BigOp op opInfo exp exp2 exp3 ->
                BigOp op opInfo (applyBetaReduce argument param exp) (applyBetaReduce argument param exp2) (applyBetaReduce argument param exp3)

            UnaryOp op opInfo exp ->
                UnaryOp op opInfo (applyBetaReduce argument param exp)

            Vector expList ->
                Vector <| List.map (\cell -> applyBetaReduce argument param cell) expList

            Matrix expListList ->
                Matrix <| List.map (List.map (\cell -> applyBetaReduce argument param cell)) expListList

            _ ->
                body


actions : List (Crum -> Maybe Exp)
actions =
    [ \x ->
        case x of
            ExpBelow (BinOp App _ argument (BinOp Abs _ param body)) ->
                applyBetaReduce argument param body |> Just |> Debug.log "found match"

            _ ->
                Nothing
    , \x ->
        case x of
            ExpBelow (BinOp Plus _ (Id right) (Id left)) ->
                case ( right |> String.toInt, left |> String.toInt ) of
                    ( Ok rightInt, Ok leftInt ) ->
                        Just <| Id <| toString <| rightInt + leftInt

                    _ ->
                        Nothing

            _ ->
                Nothing
    , \x ->
      case x of
        ExpBelow (BinOp Plus _ right left) ->
          if right == left then
            multiply (Id "2") right |> Just
          else
            Nothing
        _ -> Nothing
    , \x ->
        case x of
            ExpBelow (BinOp Sub _ (Id right) (Id left)) ->
                Nothing

            _ ->
                Nothing
    , \x ->
        case x of
            ExpBelow (BinOp Div _ (Id right) (Id left)) ->
                case ( right |> String.toFloat, left |> String.toFloat ) of
                    ( Ok rightFloat, Ok leftFloat ) ->
                        Just <| Id <| toString <| rightFloat / leftFloat

                    _ ->
                        Nothing

            _ ->
                Nothing
    ]


applyActionFolder : Crum -> Maybe Exp
applyActionFolder x =
    List.foldr
        (\action best ->
            case action x of
                Just exp ->
                    Just exp

                Nothing ->
                    best
        )
        Nothing
        actions


applyAction : BreadCrum -> Exp
applyAction =
    foldCrum applyActionFolder (Id "test")


{-| the update function. Should be called by the caller's update function updating the model:

```elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MathUiMsg msg ->
            let
                (newModel, cmds) = MathUi.update msg model.mathUi
            in
              { model | mathUi = newModel } ! [Cmd.map MathUiMsg cmds]
```
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateIdentifier breadCrum newContent ->
            { model | expression = changeIdentifier breadCrum newContent, breadCrum = breadCrum } ! [Task.attempt FocusResult (focus "mathui-focus")]

        KeyUp breadCrum key ->
            case key of
                8 ->
                    --Backspace
                    { model | expression = deleteNode breadCrum, breadCrum = breadCrum } ! []

                _ ->
                    model ! []

        ClickOn breadCrum ->
            { model | expression = applyAction breadCrum, breadCrum = breadCrum } ! []

        FocusOn id ->
          model ! [ Task.attempt FocusResult (focus id) ]

        FocusResult res ->
          let
            res2 = res |> Debug.log "res"
          in
            model ! []



onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


onClickNoPropagation : msg -> Attribute msg
onClickNoPropagation msg =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.succeed msg)


setLengthForString string =
    String.length string |> toFloat |> (*) 0.39 |> (+) 0.5 |> toString |> \x -> x ++ "em"


cls string =
    class ("mathui-" ++ string)


viewExp : Exp -> BreadCrum -> Html Msg
viewExp expression breadCrums =
    case expression of
        BinOp op opInfo left right ->
            span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn (breadCrums ++ [ ExpBelow expression ]) ]
                [ hiddenText <| "{{" ++ opInfo.latexBefore
                , span [ cls (opInfo.cssClass ++ "Left") ] [ viewExp left (breadCrums ++ [ BinOpLeft op opInfo right ]) ]
                , hiddenText <| "}" ++ opInfo.latexOperator ++ "{"
                , span [ cls (opInfo.cssClass ++ "Right") ] [ viewExp right (breadCrums ++ [ BinOpRight op opInfo left ]) ]
                , hiddenText <| opInfo.latexAfter ++ "}}"
                ]

        BigOp op opInfo under over after ->
            span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn <| breadCrums ++ [ ExpBelow expression ] ]
                [ hiddenText <| "{" ++ opInfo.latexOperator ++ opInfo.latexBefore ++ "{"
                , span [ cls (opInfo.cssClass ++ "Under") ] [ viewExp under (breadCrums ++ [ BigOpUnder op opInfo over after ]) ]
                , hiddenText <| "}" ++ opInfo.latexAfter ++ "{"
                , span [ cls (opInfo.cssClass ++ "After") ] [ viewExp after (breadCrums ++ [ BigOpAfer op opInfo under over ]) ]
                , span [ cls (opInfo.cssClass ++ "Over") ] [ viewExp over (breadCrums ++ [ BigOpOver op opInfo under after ]) ]
                , hiddenText "}}"
                ]

        UnaryOp op opInfo exp ->
            span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn (breadCrums ++ [ ExpBelow expression ]) ]
                [ hiddenText <| "{" ++ opInfo.latexOperator ++ "{"
                , span [ cls (opInfo.cssClass ++ "Inner") ] [ viewExp exp (breadCrums ++ [ UnaryOpHere op opInfo exp ]) ]
                , hiddenText "}}"
                ]

        Symbol op opInfo ->
            span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn <| breadCrums ++ [ ExpBelow expression ] ]
                [ input
                    [ value opInfo.shortName
                    , style [ ( "width", setLengthForString opInfo.shortName ) ]
                    , onKeyUp (KeyUp (breadCrums ++ [ SymbolHere op opInfo ]))
                    , onInput (UpdateIdentifier (breadCrums ++ [ SymbolHere op opInfo ]))
                    ]
                    []
                ]

        Vector expressions ->
            span [ cls "Vector", onClickNoPropagation <| ClickOn <| breadCrums ++ [ ExpBelow expression ] ]
                [ span [ cls "VectorContent" ] <|
                    List.indexedMap
                        (\index expression ->
                            span [ cls "VectorItem" ]
                                [ viewExp expression (breadCrums ++ [ VectorAt index (List.take (index) expressions) (List.drop (index + 1) expressions) ])
                                ]
                        )
                        expressions
                ]

        Matrix rows ->
            span [ cls "Matrix", onClickNoPropagation <| ClickOn <| breadCrums ++ [ ExpBelow expression ] ]
                [ table [ cls "MatrixContent" ] <|
                    List.indexedMap
                        (\indexRow row ->
                            tr [ cls "MatrixRow" ] <|
                                List.indexedMap
                                    (\indexCol cell ->
                                        viewExp cell
                                            (breadCrums
                                                ++ [ MatrixAt ( indexRow, indexCol ) (List.take indexRow rows) (List.take indexCol row) (List.drop (indexCol + 1) row) (List.drop (indexRow + 1) rows)
                                                   ]
                                            )
                                            |> List.singleton
                                            |> td [ cls "MatrixCell" ]
                                    )
                                    row
                        )
                        rows
                ]

        Id a ->
            input
                [ value a
                , style [ ( "width", setLengthForString a ) ]
                , onKeyUp (KeyUp (breadCrums ++ [ IdHere a ]))
                , onClickNoPropagation (ClickOn <| breadCrums ++ [ IdHere a ])
                , onInput (UpdateIdentifier (breadCrums ++ [ IdHere a ]))
                ]
                []

        Hole ->
            span [ cls "Hole" ]
                [ input
                    [ style [ ( "width", "0.5em" ) ]
                    , onKeyUp (KeyUp (breadCrums ++ [ HoleHere ]))
                    , onClickNoPropagation (ClickOn <| breadCrums ++ [ HoleHere ])
                    , onInput (UpdateIdentifier (breadCrums ++ [ HoleHere ]))
                    ]
                    []
                ]


{-| displays the model and lets the user change it
-}
view : Model -> Html Msg
view model =
    div [ cls "Container" ]
        [ stylesheet
        , viewExp model.expression []
        ]


{-| displays the `view` component, the latex-string and a rendering of the latex-string
-}
viewAll : Model -> Html Msg
viewAll model =
    div [ cls "Container" ]
        [ stylesheet

        --, inlinestyle
        , viewExp model.expression []
        , pre [] [ latexRepr model.expression |> text ]
        , img [ src <| "https://latex.codecogs.com/svg.latex?%5Cinline%20" ++ (latexRepr model.expression) ] []
        ]


hiddenText string =
    span [ cls "HiddenText" ] [ text string ]


inlinestyle =
    let
        tag =
            "style"
    in
        node "style" [] [ text (Css.File.compile [ MathUiCss.css ]).css ]


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "../css/elm-mathui.css"
            ]

        children =
            []
    in
        node tag attrs children
