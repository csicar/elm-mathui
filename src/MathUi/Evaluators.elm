module MathUi.Evaluators exposing (..)

import MathUi.Breadcrums exposing (..)
import MathUi.Operations exposing (..)
import Result exposing (Result(..))

type alias EvaluationResult a = Result (BreadCrum, String) a

betaReduceStep : Exp -> Exp -> Exp -> Exp
betaReduceStep argument param body =
    if body == param then
        argument |> Debug.log "arg"
    else
        case body of
            BinOp op opInfo exp exp2 ->
                BinOp op opInfo (betaReduceStep argument param exp) (betaReduceStep argument param exp2)

            BigOp op opInfo exp exp2 exp3 ->
                BigOp op opInfo (betaReduceStep argument param exp) (betaReduceStep argument param exp2) (betaReduceStep argument param exp3)

            UnaryOp op opInfo exp ->
                UnaryOp op opInfo (betaReduceStep argument param exp)

            Vector expList ->
                Vector <| List.map (\cell -> betaReduceStep argument param cell) expList

            Matrix expListList ->
                Matrix <| List.map (List.map (\cell -> betaReduceStep argument param cell)) expListList

            _ ->
                body





wolframAlphaText : Exp -> String
wolframAlphaText exp =
    case exp of
        BinOp opType opInfo left right ->
            String.concat ["", wolframAlphaText left, opInfo.shortName, wolframAlphaText right, ""]

        BigOp opType opInfo over under after ->
          String.concat [opInfo.wolframAlphaName, " (", wolframAlphaText after, "), ", wolframAlphaText under, " to ", wolframAlphaText over]

        UnaryOp opType opInfo exp ->
          String.concat ["", opInfo.shortName, wolframAlphaText exp, ""]

        Symbol opType opInfo ->
          (opInfo.shortName)

        Vector expList ->
          expList |> List.map wolframAlphaText |> String.join ", " |> \inner -> "vector {"++inner++"}"

        Matrix rows ->
          let
              reprRow row =
                  List.map wolframAlphaText row |> String.join ", "
          in
              String.concat <| [ "{{ ", String.join "}, {" (List.map reprRow rows), "}}" ]

        Id string ->
          string

        Hole ->
          "<Hole>"


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

reduceOver : (Float -> Float -> Float) -> Float -> Exp -> Float -> Float -> Exp -> EvaluationResult Float
reduceOver reducer neutral iterator from to body =
  if from > to then
    Ok neutral
  else
    let
      evaluatedIteration = betaReduceStep (from |> toString |> Id) (iterator) body |> (\x -> numMath x [])
    in

      Result.map2 reducer (evaluatedIteration) (reduceOver reducer neutral iterator (from + 1) to body)

reduceOverResult : (Float -> Float -> Float) -> Float -> Exp -> EvaluationResult Float -> EvaluationResult Float -> Exp -> EvaluationResult Float
reduceOverResult reducer neutral interator from to body =
    case (from , to) of
      (Ok fromOk, Ok toOk) ->reduceOver reducer neutral interator fromOk toOk body
      (Err err, _) -> Err err
      (_, Err err) -> Err err

factorial n =
  let
      facAcc n acc = case n of
        0 -> acc
        n -> facAcc (n-1) (acc*n)
  in
    facAcc n 1


numMath : Exp -> BreadCrum -> EvaluationResult Float
numMath exp breadCrum =
  let
      brc x = breadCrum ++ [x]
  in

    case exp of
        BinOp opType opInfo left right ->
          let
              mapperResult = case opType of
                Plus -> Ok (+)
                Sub -> Ok (-)
                Mul ->Ok (*)
                Div -> Ok (/)
                Pow -> Ok (^)
                _ -> Err ((ExpBelow exp |> brc), "invalid operator")
          in
            case mapperResult of
              Ok mapper -> Result.map2 mapper (numMath left (BinOpLeft opType opInfo right |> brc)) (numMath right (BinOpRight opType opInfo left |> brc))
              Err err -> Err err

        UnaryOp opType opInfo inner ->
          let
              mapperResult = case opType of
                Root n -> Ok (\v -> v ^ (1/(toFloat n)))
                Factorial -> Ok (round >> factorial >> toFloat)
                _ -> Err ((ExpBelow exp |> brc), "invalid operator")
          in
            case mapperResult of
              Ok mapper -> Result.map mapper (numMath inner (UnaryOpHere opType opInfo inner |> brc))
              Err err -> Err err

        BigOp opType opInfo to (BinOp Equals eqInfo iterator from) over ->
          let
              breadCrumFrom = breadCrum ++ [(BigOpUnder opType opInfo to over), (BinOpRight Equals eqInfo iterator)]
              breadCrumTo = breadCrum ++ [(BigOpOver opType opInfo from over)]
              mapperResult = case opType of
                BigSum -> (\fromVal toVal -> reduceOverResult (+) 0 iterator fromVal toVal over) (numMath from breadCrumFrom) (numMath to breadCrumTo)
                BigProd -> (\fromVal toVal -> reduceOverResult (*) 1 iterator fromVal toVal over) (numMath from breadCrumFrom) (numMath to breadCrumTo)
                _ -> Err ((ExpBelow exp |> brc), "invalid operator")
          in
            mapperResult


        Id val ->
          String.toFloat val |> Result.mapError (\msg -> (breadCrum ++ [IdHere val], msg))
        s -> Result.Err (breadCrum, (Debug.log "not impl" s ) |> toString)
