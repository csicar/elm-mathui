module MathUi.Operations exposing (..)
{-|


Operators
----

@docs plus, divide, pow, elemIn, equals, sqrtOp, parentheses, sigma, infinity, vectorsymbol, app, lambda
-}
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
