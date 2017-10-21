module MathUi exposing (OpInfo, Exp(..), Model, Msg, update, view, viewAll, latexRepr, plus, divide, pow, elemIn, equals, sqrtOp, parentheses, sigma, infinity, vectorsymbol)

{-| MathUi


# Types

@docs OpInfo, Exp, Model, Msg


# Update

@docs update, view, viewAll, latexRepr


# Configuration


##


## build-in constructs

@docs plus, divide, pow, elemIn, equals, sqrtOp, parentheses, sigma, infinity, vectorsymbol

-}

import Html exposing (Html, Attribute, div, input, text, span, node, pre, img, table, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode, onClick, onWithOptions)
import Json.Decode as Json
import Css.File exposing (CssFileStructure, CssCompilerProgram)
import MathUiCss


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type LatexOperator = Operator String | AdvancedOperator String

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


{-| Homogeneous syntax tree
-}
type Exp
    = BinOp OpInfo Exp Exp
    | BigOp OpInfo Exp Exp Exp
    | UnaryOp OpInfo Exp
    | Symbol OpInfo
    | Vector (List Exp)
    | Matrix (List (List Exp))
    | Id String
    | Hole



plusInfo = { shortName = "+", longName = "plus", cssClass = "Plus", latexOperator = "+", latexBefore = "", latexAfter = "" }

{-| plus left right
-}
plus : Exp -> Exp -> Exp
plus =
    BinOp plusInfo


{-| multiply left right
-}
multiply : Exp -> Exp -> Exp
multiply =
    BinOp { shortName = "*", longName = "multiply", cssClass = "Multiply", latexOperator = "\\cdot", latexBefore = "", latexAfter = "" }


{-| divide top bottom
-}
divide : Exp -> Exp -> Exp
divide =
    BinOp { shortName = "/", longName = "divide", cssClass = "Div", latexOperator = "\\over", latexBefore = "", latexAfter = "" }


{-| pow basis exponent
-}
pow : Exp -> Exp -> Exp
pow =
    BinOp { shortName = "^", longName = "power", cssClass = "Pow", latexOperator = "^", latexBefore = "", latexAfter = "" }

sub : Exp -> Exp -> Exp
sub =
  BinOp {shortName="_", longName = "subscript", cssClass="Sub", latexOperator="_", latexBefore="", latexAfter=""}


{-| elemIn set item
-}
elemIn : Exp -> Exp -> Exp
elemIn =
    BinOp { shortName = "∈", longName = "in", cssClass = "ElemIn", latexOperator = "\\in", latexBefore = "", latexAfter = "" }


{-| equals left right
-}
equals : Exp -> Exp -> Exp
equals =
    BinOp { shortName = "=", longName = "equals", cssClass = "Equals", latexOperator = "=", latexBefore = "", latexAfter = "" }


{-| functionApplication func argument
-}
functionApplication : Exp -> Exp -> Exp
functionApplication =
    BinOp { shortName = "⇒", longName = "functionApplication", cssClass = "FunctionApplication", latexOperator = "", latexBefore = "", latexAfter = "" }

lambda : Exp -> Exp -> Exp
lambda =
  BinOp {shortName="λ", longName="lambda", cssClass="Lambda", latexOperator="", latexBefore="", latexAfter=""}

appInfo = {shortName="β", longName="app", cssClass="App", latexOperator="", latexBefore="", latexAfter=""}

app : Exp -> Exp -> Exp
app =
  BinOp appInfo

{-| sqrtOp operand
-}
sqrtOp : Exp -> Exp
sqrtOp =
    UnaryOp { shortName = "√", longName = "sqrt", cssClass = "Sqrt", latexOperator = "\\sqrt", latexBefore = "", latexAfter = "" }


{-| enclose expression in parentheses
-}
parentheses : Exp -> Exp
parentheses =
    UnaryOp { shortName = "()", longName = "parentheses", cssClass = "Parentheses", latexOperator = "()", latexBefore = "(", latexAfter = ")" }


{-| place vector arrow above expression
-}
vectorsymbol : Exp -> Exp
vectorsymbol =
    UnaryOp { shortName = "vec", longName = "→", cssClass = "VectorSymbol", latexOperator = "\\vec", latexBefore = "", latexAfter = "" }


{-| sigma from to over
-}
sigma : Exp -> Exp -> Exp -> Exp
sigma =
    BigOp { shortName = "Σ", longName = "sigma", cssClass = "Sigma", latexOperator = "\\sum", latexBefore = "_", latexAfter = "^" }


{-| infinity symbol
-}
infinity : Exp
infinity =
    Symbol { shortName = "∞", longName = "infinity", cssClass = "Infinity", latexOperator = "\\infty", latexBefore = "", latexAfter = "" }



options =
    [ ( "+", \rest -> plus (Id rest) Hole )
    , ( "*", \rest -> multiply (Id rest) Hole )
    , ( "/", \rest -> divide (Id rest) Hole )
    , ( "^", \rest -> pow (Id rest) Hole )
    , ( "_", \rest -> sub (Id rest) Hole )
    , ( "=", \rest -> equals (Id rest) Hole )
    , ( "\\lambda", \rest -> lambda (Id rest) (Hole))
    , ( "\\app", \rest -> app (Id rest) (Hole))
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
        BinOp info a b ->
            String.concat [ "{", info.latexBefore, "{", latexRepr a, "}", info.latexOperator, "{", latexRepr b, "}", info.latexAfter, "}" ]

        BigOp info under over exp ->
            String.concat [ "{", info.latexOperator, info.latexBefore, "{", latexRepr under, "}", info.latexAfter, "{", latexRepr over, "}", "{", latexRepr exp, "}}" ]

        UnaryOp info exp ->
            String.concat [ "{", info.latexBefore, info.latexOperator, "{", latexRepr exp, "}", info.latexAfter, "}" ]

        Symbol info ->
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
    = BinOpLeft OpInfo Exp
    | BinOpRight OpInfo Exp
    | BigOpUnder OpInfo Exp Exp
    | BigOpOver OpInfo Exp Exp
    | BigOpAfer OpInfo Exp Exp
    | UnaryOpHere OpInfo Exp
    | SymbolHere OpInfo
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
      (x::[]) -> f x |> Maybe.withDefault (case x of
        SymbolHere opInfo -> Symbol opInfo
        ExpBelow exp -> exp
        IdHere s -> Id s
        HoleHere -> Hole
        _ -> neutral
          )
      x :: xs ->
          let
              futureExp = foldCrum f neutral xs |> Debug.log "exp"
          in
              case x of
                  BinOpLeft opInfo exp ->
                      BinOp opInfo futureExp exp

                  BinOpRight opInfo exp ->
                      BinOp opInfo exp futureExp

                  BigOpUnder opInfo over after ->
                      BigOp opInfo futureExp over after

                  BigOpOver opInfo under after ->
                      BigOp opInfo under futureExp after

                  BigOpAfer opInfo under over ->
                      BigOp opInfo under over futureExp

                  UnaryOpHere opInfo inner ->
                      UnaryOp opInfo futureExp

                  VectorAt pos before after ->
                      Vector (before ++ [ futureExp ] ++ after)

                  MatrixAt pos rowsBefore cellsBefore cellsAfter rowsAfter ->
                      rowsBefore ++ [ cellsBefore ++ [ futureExp ] ++ cellsAfter ] ++ rowsAfter |> Matrix

                  SymbolHere opInfo ->
                      Symbol opInfo

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
    newExp = textToExp newContent |> Just
  in
    case crum of

        IdHere string ->
          newExp

        SymbolHere _ ->
          newExp

        HoleHere ->
          newExp


        _ -> Nothing


changeIdentifier : BreadCrum -> String -> Exp
changeIdentifier breadCrum newContent = foldCrum (changeIdentifierFolder newContent) Hole breadCrum


deletePartWithOption : BreadCrum -> Crum -> Exp -> ( Exp, Exp )
deletePartWithOption breadCrum crum changedExp =
    case crum of
        BinOpLeft opInfo exp ->
            ( BinOp opInfo changedExp exp, exp )

        BinOpRight opInfo exp ->
            ( BinOp opInfo exp changedExp, exp )

        BigOpUnder opInfo over after ->
            ( BigOp opInfo changedExp over after, after )

        BigOpOver opInfo under after ->
            ( BigOp opInfo under changedExp after, after )

        BigOpAfer opInfo under over ->
            ( BigOp opInfo under over changedExp, Hole )

        UnaryOpHere opInfo _ ->
            ( UnaryOp opInfo changedExp, Hole )

        VectorAt pos before after ->
            ( Vector (before ++ [ changedExp ] ++ after), Vector (before ++ after) )

        MatrixAt pos rowsBefore cellsBefore cellsAfter rowsAfter ->
            ( rowsBefore ++ [ cellsBefore ++ [ changedExp ] ++ cellsAfter ] ++ rowsAfter |> Matrix, rowsBefore ++ rowsAfter |> Matrix )

        SymbolHere opInfo ->
            ( Hole, Hole )

        IdHere "" ->
            ( Hole, Hole )

        IdHere s ->
            ( Id s, Id s )

        ExpBelow exp ->
            (exp, Hole)

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


applyActionFolder : Crum -> Maybe Exp
applyActionFolder x =
  case x of
    HoleHere -> Id "yay! not a hole anymore!" |> Just
    _ -> Nothing

applyAction : BreadCrum -> Exp
applyAction = foldCrum applyActionFolder (Id "test")



{-| the update function. Should be called by the caller's update function updating the model:

    update : Msg -> Model -> Model
    update msg model =
        case msg of
            MathUiMsg msg ->
                { model | mathUi = MathUi.update msg model.mathUi }

-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateIdentifier breadCrum newContent ->
            { model | expression = changeIdentifier breadCrum newContent, breadCrum = breadCrum }

        KeyUp breadCrum key ->
            case key of
                8 ->
                    --Backspace
                    { model | expression = deleteNode breadCrum, breadCrum = breadCrum }

                _ ->
                    model
        ClickOn breadCrum ->
            {model | expression = applyAction breadCrum, breadCrum = breadCrum}


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)

onClickNoPropagation : msg -> Attribute msg
onClickNoPropagation msg = onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.succeed msg)

setLengthForString string =
    String.length string |> toFloat |> (*) 0.39 |> (+) 0.5 |> toString |> \x -> x ++ "em"


cls string =
    class ("mathui-" ++ string)


viewExp : Exp -> BreadCrum -> Html Msg
viewExp expression breadCrums =
    case expression of
        BinOp opInfo left right ->
            span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn (breadCrums ++ [ ExpBelow expression ]) ]
                [ hiddenText <| "{{" ++ opInfo.latexBefore
                , span [ cls (opInfo.cssClass ++ "Left") ] [ viewExp left (breadCrums ++ [ BinOpLeft opInfo right ]) ]
                , hiddenText <| "}" ++ opInfo.latexOperator ++ "{"
                , span [ cls (opInfo.cssClass ++ "Right") ] [ viewExp right (breadCrums ++ [ BinOpRight opInfo left ]) ]
                , hiddenText <| opInfo.latexAfter ++ "}}"
                ]

        BigOp opInfo under over after ->
            span [ cls opInfo.cssClass,  onClickNoPropagation <| ClickOn <| breadCrums ++ [ExpBelow expression]  ]
                [ hiddenText <| "{" ++ opInfo.latexOperator ++ opInfo.latexBefore ++ "{"
                , span [ cls (opInfo.cssClass ++ "Under") ] [ viewExp under (breadCrums ++ [ BigOpUnder opInfo over after ]) ]
                , hiddenText <| "}" ++ opInfo.latexAfter ++ "{"
                , span [ cls (opInfo.cssClass ++ "After") ] [ viewExp after (breadCrums ++ [ BigOpAfer opInfo under over ]) ]
                , span [ cls (opInfo.cssClass ++ "Over") ] [ viewExp over (breadCrums ++ [ BigOpOver opInfo under after ]) ]
                , hiddenText "}}"
                ]

        UnaryOp opInfo exp ->
            span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn (breadCrums ++ [ ExpBelow expression ]) ]
                [ hiddenText <| "{" ++ opInfo.latexOperator ++ "{"
                , span [ cls (opInfo.cssClass ++ "Inner") ] [ viewExp exp (breadCrums ++ [ UnaryOpHere opInfo exp ]) ]
                , hiddenText "}}"
                ]

        Symbol opInfo ->
            span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn <| breadCrums ++ [ExpBelow expression]  ]
                [ input
                    [ value opInfo.shortName
                    , style [ ( "width", setLengthForString opInfo.shortName ) ]
                    , onKeyUp (KeyUp (breadCrums ++ [ SymbolHere opInfo ]))
                    , onInput (UpdateIdentifier (breadCrums ++ [ SymbolHere opInfo ]))
                    ]
                    []
                ]

        Vector expressions ->
            span [ cls "Vector", onClickNoPropagation <| ClickOn <| breadCrums ++ [ExpBelow expression] ]
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
            span [ cls "Matrix" ,  onClickNoPropagation <| ClickOn <| breadCrums ++ [ExpBelow expression] ]
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
                , onClickNoPropagation (ClickOn <| breadCrums ++ [IdHere a])
                , onInput (UpdateIdentifier (breadCrums ++ [ IdHere a ]))
                ]
                []

        Hole ->
            span [ cls "Hole" ]
                [ input
                  [ style [ ( "width", "0.5em" ) ]
                  , onKeyUp (KeyUp (breadCrums ++ [ HoleHere ]))
                  , onClickNoPropagation (ClickOn <| breadCrums ++ [HoleHere])
                  , onInput (UpdateIdentifier (breadCrums ++ [ HoleHere ])) ] []
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
