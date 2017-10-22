module MathUi.Evaluators exposing (..)

import MathUi.Breadcrums exposing (..)
import MathUi.Operations exposing (..)
import Result exposing (Result(..))


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

type NumMathValue = NInt Int | NFloat Float | NBool Bool

type alias Koerper a = {
  toString: a -> String,
  fromString: String -> Result.Result String a,
  add: a -> a -> a,
  negate: a -> a,
  multiply: a -> a -> a,
  inverse: a -> a,
  eins: a,
  null: a
  }

koerperFloat : Koerper Float
koerperFloat = {
  toString= \a -> toString a,
  fromString= \s -> String.toFloat s,
  add= \a b -> a + b,
  negate= \a -> (0 - a),
  multiply= \a b -> a*b,
  inverse= \a -> 1 / a,
  eins= 1,
  null=0
  }

mapResultTuple : (a -> b -> r) -> Result e a-> Result e b -> Result e r
mapResultTuple mapper a b =
    case (a, b) of
      (Ok av, Ok bv) -> Ok (mapper av bv)
      (_, Err err) -> Err err
      (Err err, _) -> Err err

type alias EvaluationResult a = Result (BreadCrum, String) a

sumOver : Exp -> Float -> Float -> Exp -> EvaluationResult Float
sumOver iterator from to body =
  if from > to then
    Ok 0
  else
    let
      evaluatedIteration = betaReduceStep (from |> toString |> Id) (iterator) body |> (\x -> numMath x [])
    in

      Result.map2 (+) (evaluatedIteration) (sumOver iterator (from + 1) to body)

sumOverResult : Exp -> EvaluationResult Float -> EvaluationResult Float -> Exp -> EvaluationResult Float
sumOverResult interator from to body =
    case (from , to) of
      (Ok fromOk, Ok toOk) ->sumOver interator fromOk toOk body
      (Err err, _) -> Err err
      (_, Err err) -> Err err


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
                BigSum -> (\fromVal toVal -> sumOverResult iterator fromVal toVal over) (numMath from breadCrumFrom) (numMath to breadCrumTo)
                _ -> Err ((ExpBelow exp |> brc), "invalid operator")
          in
            mapperResult


        Id val ->
          String.toFloat val |> Result.mapError (\msg -> (breadCrum ++ [IdHere val], msg))
        s -> Result.Err (breadCrum, (Debug.log "not impl" s ) |> toString)

        --
        -- BigOp opType opInfo exp exp2 exp3 ->
        --
        --
        -- UnaryOp opType opInfo exp ->
        --
        --
        -- Symbol opType opInfo ->
        --
        --
        -- Vector expList ->
        --
        --
        -- Matrix expListList ->
        --
        --
        -- Id string ->
        --
        --
        -- Hole ->
