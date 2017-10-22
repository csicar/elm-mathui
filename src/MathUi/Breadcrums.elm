module MathUi.Breadcrums exposing (..)

import MathUi.Operations exposing (..)


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
