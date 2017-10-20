module MathUiCss exposing (..)

{-| @docs CssIds, CssClasses, css
-}

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (..)


{-| -}
type CssIds
    = Bottom


{-| -}
type CssClasses
    = Wrapper
    | Div
    | DivLeft
    | DivRight
    | PlusRight
    | PlusLeft
    | Plus
    | Hole
    | Equals
    | EqualsRight
    | EqualsLeft
    | Sigma
    | SigmaUnder
    | SigmaOver
    | SigmaAfter
    | Pow
    | PowRight
    | PowLeft
    | Matrix
    | MatrixContent
    | Vector
    | VectorItem
    | ElemIn
    | Sqrt
    | VectorSymbol
    | Parentheses
    | HiddenText
    | FunctionApplication


{-| -}
css : Css.Stylesheet
css =
    (stylesheet << namespace "mathui-")
        [ class Wrapper
            [ fontSize (Css.em 3)
            ]
        , class Div
            [ display inlineBlock
            , verticalAlign middle
            , children
                [ typeSelector "*"
                    [ display block
                    , textAlign center
                    , fontSize (Css.em 0.7)
                    ]
                , class DivLeft
                    [ borderBottom (px 1)
                    ]
                ]
            ]
        , class Plus
            [ children
                [ class PlusRight
                    [ before
                        [ property "content" "'+'"
                        , verticalAlign middle
                        ]
                    ]
                ]
            ]
        , class Equals
            [ children
                [ class EqualsRight
                    [ before
                        [ property "content" "'='"
                        ]
                    ]
                ]
            ]
        , class Sigma
            [ display inlineBlock
            , verticalAlign middle
            , children
                [ typeSelector "*"
                    [ display block
                    ]
                , class SigmaUnder
                    [ fontSize (Css.em 0.7)
                    , lineHeight (Css.em 0)
                    , height (Css.em 0.6)
                    ]
                , class SigmaOver
                    [ fontSize (Css.em 0.7)
                    , lineHeight (Css.em 0)
                    , height (Css.em 0.6)
                    ]
                , class SigmaAfter
                    [ before [ property "content" "'Σ'", fontSize (Css.em 2), verticalAlign middle ]
                    , verticalAlign middle
                    ]
                ]
            ]
        , class Pow
            [ children
                [ class PowRight
                    [ verticalAlign super
                    , fontSize (Css.em 0.8)
                    ]
                ]
            ]
        , class Matrix
            [ children
                [ class MatrixContent
                    [ display inlineTable
                    , verticalAlign middle
                    , padding2 (Css.em 0) (Css.em 0.4)
                    , borderLeft (Css.em 0.08)
                    , borderRight (Css.em 0.08)
                    , borderTopLeftRadius2 (pct 30) (pct 100)
                    , borderBottomLeftRadius2 (pct 30) (pct 100)
                    , borderTopRightRadius2 (pct 30) (pct 100)
                    , borderBottomRightRadius2 (pct 30) (pct 100)
                    ]
                ]
            ]
        , class Vector
            [ display inlineBlock
            , verticalAlign middle
            , borderLeft (Css.em 0.08)
            , borderRight (Css.em 0.08)
            , borderTop3 (Css.em 0.08) solid transparent
            , borderBottom3 (Css.em 0.08) solid transparent
            , borderRadius2 (pct 51) (pct 13)
            ]
        , class Hole
            [ children
                [ input [ boxShadow5 (px 0) (px 0) (px 0) (px 1) (hex "#bada55") ]
                ]
            ]
        , input
            [ fontSize (Css.em 1)
            , textAlign center
            , backgroundColor transparent
            , borderStyle none
            , boxShadow5 (px 0) (px 0) (px 0) (px 1) (hex "#72dfff")
            ]
        , class HiddenText
            [ overflow hidden
            , fontSize (px 0)
            ]
        ]
