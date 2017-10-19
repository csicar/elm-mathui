import Html exposing (Html, Attribute, div, input, text, span, node, pre, img, table, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json

main =
  Html.beginnerProgram { model = model, view = view, update = update }




type alias OpInfo =
  { shortName: String
  , longName: String
  , cssClass: String
  , latexOperator: String
  , latexBefore: String
  , latexAfter: String
  }

type Exp
  = BinOp OpInfo Exp Exp
  | BigOp OpInfo Exp Exp Exp
  | UnaryOp OpInfo Exp
  | Symbol OpInfo
  | Vector (List Exp)
  | Matrix (List (List Exp))
  | Id String
  | Hole

plus = BinOp {shortName= "+", longName= "plus", cssClass= "plus", latexOperator= "+", latexBefore="", latexAfter=""}
divide = BinOp {shortName= "/", longName= "divide", cssClass= "div", latexOperator= "\\over", latexBefore="", latexAfter=""}
pow = BinOp {shortName="^", longName="power", cssClass="pow", latexOperator= "^", latexBefore="", latexAfter=""}
elemIn = BinOp {shortName="∈", longName="in", cssClass="elem-in", latexOperator= "\\in", latexBefore="", latexAfter=""}
equals = BinOp {shortName="=", longName="equals", cssClass="equals", latexOperator="=", latexBefore="", latexAfter=""}

sqrtOp = UnaryOp {shortName="√", longName="sqrt", cssClass="sqrt", latexOperator="\\sqrt", latexBefore="",latexAfter=""}
parentheses = UnaryOp {shortName="()", longName="parentheses", cssClass="parentheses", latexOperator="()", latexBefore="(", latexAfter=")"}

sigma = BigOp {shortName="Σ", longName="sigma", cssClass="sigma", latexOperator= "\\sum", latexBefore="_", latexAfter="^"}

infinity = Symbol {shortName="∞", longName="infinity", cssClass="infinity", latexOperator= "\\infty",latexBefore="", latexAfter=""}


options = [
  ("+", \rest -> plus (Id rest) Hole),
  ("/", \rest -> divide (Id rest) Hole),
  ("^", \rest -> pow (Id rest) Hole),
  ("=", \rest -> equals (Id rest) Hole),
  ("\\in", \rest -> elemIn (Id rest) Hole),
  ("\\sqrt", \rest -> sqrtOp (Id rest)),
  ("(", \rest -> parentheses (Id rest)),
  ("\\infinity", \_ -> infinity),
  ("\\sigma", \rest -> sigma Hole Hole (Id rest)),
  ("\\v2", \rest -> Vector [Id rest, Hole]),
  ("\\v3", \rest -> Vector [Id rest, Hole, Hole]),
  ("\\vv", \rest -> case (String.toInt rest) of
      Ok value -> Vector <| List.repeat value Hole
      Err _ -> Vector [Id rest, Hole]
    ),
  ("\\m3", \rest -> List.repeat 3 () |> List.map (\a -> List.repeat 3 Hole) |> Matrix)
  ]


latexRepr : Exp -> String
latexRepr exp =
  case exp of
    BinOp info a b ->
      String.concat ["{", info.latexBefore, "{", latexRepr a, "}", info.latexOperator, "{", latexRepr b, "}", info.latexAfter, "}"]

    BigOp info under over exp ->
      String.concat ["{", info.latexOperator, info.latexBefore,"{", latexRepr under, "}", info.latexAfter, "{", latexRepr over, "}", "{", latexRepr exp, "}}"]
    UnaryOp info exp ->
      String.concat ["{", info.latexBefore, info.latexOperator, "{", latexRepr exp, "}", info.latexAfter, "}"]
    Symbol info ->
      String.concat ["{", info.latexBefore, info.latexOperator, info.latexAfter, "}"]
    Vector exps ->
      String.concat <| ["\\begin{bmatrix} " ] ++ List.map (\val -> (latexRepr val) ++ " \\\\ ") exps ++ [" \\end{bmatrix}"]
    Matrix rows -> let
        reprRow row = List.map latexRepr row |> String.join " & "
    in

      String.concat <| ["\\begin{pmatrix} ", String.join  "\\\\" (List.map reprRow rows) , "\\end{pmatrix}"]

    Id string ->
      String.concat ["{", string, "}"]
    Hole -> "<Hole>"

type Crum =
  BinOpLeft OpInfo Exp
  | BinOpRight OpInfo Exp
  | BigOpUnder OpInfo Exp Exp
  | BigOpOver OpInfo Exp Exp
  | BigOpAfer OpInfo Exp Exp
  | UnaryOpHere OpInfo Exp
  | SymbolHere OpInfo
  | VectorAt Int (List Exp) (List Exp)
  | MatrixAt (Int, Int) (List (List Exp)) (List Exp) (List Exp) (List (List Exp))
  | IdHere String
  | HoleHere

type alias BreadCrum = List Crum

-- MODEL
type alias Model =
  {
    expression: Exp,
    breadCrum: BreadCrum
  }

model : Model

model =
  { expression = plus (
      sigma
        (equals (Id "i") (Id "2"))
        (infinity)
        (plus
          (Id "a")
          (divide (Id "c") (sqrtOp (Id "2")))
        )
      )
        (elemIn
          (Vector [Id "a", Id "b", Id "c"])
          (Id "R")
        )
      ,
  breadCrum = []}

model2 = {
  expression = Hole,
  breadCrum = []
  }
-- UPDATE

type Msg
  = UpdateIdentifier BreadCrum String
  | KeyUp BreadCrum Int

textToExp : String -> Exp
textToExp string =
  let
      matchingOptions =
        List.filter (\(marker, constructor) -> String.endsWith marker string) options
  in
    case List.head matchingOptions of
      Just (marker, constructor) -> constructor <| String.left ((String.length string) - (String.length marker)) string
      Nothing -> Id string

changeIdentifier : BreadCrum -> String -> Exp
changeIdentifier breadCrum newContent =
  case breadCrum of
    (x::xs) ->
      let
          changedExp = changeIdentifier xs newContent
          newExp = textToExp newContent
      in
        case x of
          BinOpLeft opInfo exp -> BinOp opInfo changedExp exp
          BinOpRight opInfo exp -> BinOp opInfo exp changedExp
          BigOpUnder opInfo over  after -> BigOp opInfo changedExp over after
          BigOpOver  opInfo under after -> BigOp opInfo under changedExp after
          BigOpAfer  opInfo under over  -> BigOp opInfo under over changedExp
          UnaryOpHere opInfo _ -> UnaryOp opInfo changedExp
          VectorAt pos before after -> Vector (before ++ [changedExp] ++ after)
          MatrixAt pos rowsBefore cellsBefore cellsAfter rowsAfter -> rowsBefore ++ [cellsBefore ++ [changedExp] ++ cellsAfter] ++ rowsAfter |> Matrix
          SymbolHere opInfo -> newExp
          IdHere _ -> newExp
          HoleHere -> newExp
    [] -> Hole


update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateIdentifier breadCrum newContent ->
      { model | expression = changeIdentifier breadCrum newContent, breadCrum = breadCrum}
    KeyUp breadCrum key ->
      model
      -- case key of
      --   8 -> --Backspace
      --     { model | expression = deleteNode breadCrum, breadCrum = breadCrum}
      --   _ -> model



-- VIEW

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
  on "keyup" (Json.map tagger keyCode)

setLengthForString string =
  String.length string |> toFloat |> (*) 0.39 |> (+) 0.5 |> toString |> \x -> x++"em"



viewExp : Exp -> BreadCrum -> Html Msg
viewExp expression breadCrums =
  case expression of
    BinOp opInfo left right ->
      span [class opInfo.cssClass] [
        hiddenText <| "{{"++opInfo.latexBefore,
        span [class (opInfo.cssClass++"-left")] [viewExp left (breadCrums ++ [BinOpLeft opInfo right])],
        hiddenText <| "}"++opInfo.latexOperator++"{",
        span [class (opInfo.cssClass++"-right")] [viewExp right (breadCrums ++ [BinOpRight opInfo left])],
        hiddenText <| opInfo.latexAfter++"}}"
      ]

    BigOp opInfo under over after ->
      span [class opInfo.cssClass] [
        hiddenText <| "{"++opInfo.latexOperator++opInfo.latexBefore++"{",
        span [class (opInfo.cssClass++"-under")] [viewExp under (breadCrums ++ [BigOpUnder opInfo over after])],
        hiddenText <| "}"++opInfo.latexAfter++"{",
        span [class (opInfo.cssClass++"-after")] [viewExp after (breadCrums ++ [BigOpAfer opInfo under over])],
        span [class (opInfo.cssClass++"-over")] [viewExp over (breadCrums ++ [BigOpOver opInfo under after])],
        hiddenText "}}"
      ]

    UnaryOp opInfo exp ->
      span [class opInfo.cssClass] [
        hiddenText <| "{"++opInfo.latexOperator++"{",
        span [class (opInfo.cssClass++"-inner")] [viewExp exp (breadCrums ++ [UnaryOpHere opInfo exp])],
        hiddenText "}}"
      ]

    Symbol opInfo ->
      span [class opInfo.cssClass] [
        input [
          value opInfo.shortName,
          style [("width", setLengthForString opInfo.shortName)],
          onKeyUp (KeyUp (breadCrums ++ [SymbolHere opInfo])),
          onInput (UpdateIdentifier (breadCrums ++ [SymbolHere opInfo]))
          ] []
      ]

    Vector expressions ->
      span [class "vector"] [
        span [class "vector-content"] <|
          List.indexedMap (\index expression ->
            span [class "vector-item"] [
            viewExp expression (breadCrums ++ [VectorAt index (List.take (index) expressions) (List.drop (index+1) expressions)])
            ]
            ) expressions
      ]

    Matrix rows ->
      span [class "matrix"] [
        table [class "matrix-content"] <|
          List.indexedMap (\indexRow row ->
            tr [class "matrix-row"] <|
              List.indexedMap (\indexCol cell ->
                viewExp cell (breadCrums ++ [
                  MatrixAt (indexRow, indexCol) (List.take indexRow rows) (List.take indexCol row) (List.drop (indexCol+1) row) (List.drop (indexRow +1) rows)
                  ]) |> List.singleton |> td [class "matrix-cell"]
              ) row

          ) rows
      ]


    Id a ->
      input [value a,
        style [("width", setLengthForString a)],
        onKeyUp (KeyUp (breadCrums ++ [IdHere a])),
        onInput (UpdateIdentifier (breadCrums ++ [IdHere a]))
      ] []

    Hole ->
      input [style [("width", "3px")], onInput (UpdateIdentifier (breadCrums ++ [HoleHere]))] []


view : Model -> Html Msg
view model =
  div []
    [ stylesheet
    , viewExp model.expression []
    , pre [] [latexRepr model.expression |> text]
    , img [src <| "https://latex.codecogs.com/svg.latex?%5Cinline%20"++(latexRepr model.expression)] []
    ]


hiddenText string =
  span [class "hidden-text"] [text string]

stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "./zipperHom.css"
            ]
        children = []
    in
        node tag attrs children
