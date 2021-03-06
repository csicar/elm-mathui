module MathUi exposing (Model, Msg, update, view, viewAll)

{-| MathUi


# Types

@docs Model, Msg


# Update

@docs update, view, viewAll


# Configuration


##

-}

import Html exposing (Html, Attribute, div, input, text, span, node, pre, img, table, tr, td, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode, onClick, onWithOptions)
import Json.Decode as Json
import Css.File exposing (CssFileStructure, CssCompilerProgram)
import Dom exposing (focus)
import Task
import Bitwise exposing (shiftLeftBy)
import Char
import MathUi.Operations exposing (..)
import MathUi.Breadcrums exposing (..)
import MathUi.Evaluators exposing (..)


main =
    Html.program { init = ( model, Cmd.none ), view = view, update = update, subscriptions = \x -> Sub.none }


type LatexOperator
    = Operator String
    | AdvancedOperator String


{-| -}
type alias Model =
    { expression : Exp
    , breadCrum : BreadCrum
    , focus : BreadCrum
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
    , focus = []
    }


model2 =
    { expression = Hole
    , breadCrum = []
    , focus = []
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


actions : List (Crum -> Maybe Exp)
actions =
    [ \x ->
        case x of
            ExpBelow (BinOp App _ argument (BinOp Abs _ param body)) ->
                betaReduceStep argument param body |> Just |> Debug.log "found match"

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

            _ ->
                Nothing
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

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            MathUiMsg msg ->
                let
                    ( newModel, cmds ) =
                        MathUi.update msg model.mathUi
                in
                    { model | mathUi = newModel } ! [ Cmd.map MathUiMsg cmds ]

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateIdentifier breadCrum newContent ->
            { model | expression = changeIdentifier breadCrum newContent, breadCrum = breadCrum, focus = breadCrum } ! [ Task.attempt FocusResult (focus "mathui-focus") ]

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
                res2 =
                    res |> Debug.log "res"
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


closelyMatches : Crum -> Crum -> Bool
closelyMatches a b =
    case a of
        IdHere _ ->
            case b of
                IdHere _ ->
                    True

                HoleHere ->
                    True

                _ ->
                    False

        HoleHere ->
            case b of
                IdHere _ ->
                    True

                HoleHere ->
                    True

                _ ->
                    False

        _ ->
            False


viewExp : Exp -> Maybe BreadCrum -> BreadCrum -> Html Msg
viewExp expression focus breadCrums =
    let
        focusMe =
            Maybe.map (\l -> List.length l == 1) focus |> Maybe.withDefault False

        focusTail =
            case focus of
                Just a ->
                    (List.tail a)

                Nothing ->
                    Nothing

        shortenedFocusTail item =
            case focusTail of
                Just (x :: xs) ->
                    if (closelyMatches x item) then
                        Just xs
                    else
                        Just []

                _ ->
                    Nothing

        shouldFocusId =
            if focusMe then
                "mathui-focus"
            else
                "mathui-blur"

        viewExpAutoBreadCrum exp crum =
            viewExp exp (shortenedFocusTail crum) (breadCrums ++ [ crum ])
    in
        case expression of
            BinOp op opInfo left right ->
                span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn (breadCrums ++ [ ExpBelow expression ]) ]
                    [ hiddenText <| "{{" ++ opInfo.latexBefore
                    , span [ cls (opInfo.cssClass ++ "Left") ] [ viewExpAutoBreadCrum left <| BinOpLeft op opInfo right ]
                    , hiddenText <| "}" ++ opInfo.latexOperator ++ "{"
                    , span [ cls (opInfo.cssClass ++ "Right") ] [ viewExpAutoBreadCrum right <| BinOpRight op opInfo left ]
                    , hiddenText <| opInfo.latexAfter ++ "}}"
                    ]

            BigOp op opInfo under over after ->
                span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn <| breadCrums ++ [ ExpBelow expression ] ]
                    [ hiddenText <| "{" ++ opInfo.latexOperator ++ opInfo.latexBefore ++ "{"
                    , span [ cls (opInfo.cssClass ++ "Under") ] [ viewExp under focusTail (breadCrums ++ [ BigOpUnder op opInfo over after ]) ]
                    , hiddenText <| "}" ++ opInfo.latexAfter ++ "{"
                    , span [ cls (opInfo.cssClass ++ "After") ] [ viewExp after focusTail (breadCrums ++ [ BigOpAfer op opInfo under over ]) ]
                    , span [ cls (opInfo.cssClass ++ "Over") ] [ viewExp over focusTail (breadCrums ++ [ BigOpOver op opInfo under after ]) ]
                    , hiddenText "}}"
                    ]

            UnaryOp op opInfo exp ->
                span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn (breadCrums ++ [ ExpBelow expression ]) ]
                    [ hiddenText <| "{" ++ opInfo.latexOperator ++ "{"
                    , span [ cls (opInfo.cssClass ++ "Inner") ] [ viewExp exp focusTail (breadCrums ++ [ UnaryOpHere op opInfo exp ]) ]
                    , hiddenText "}}"
                    ]

            Symbol op opInfo ->
                span [ cls opInfo.cssClass, onClickNoPropagation <| ClickOn <| breadCrums ++ [ ExpBelow expression ] ]
                    [ input
                        [ value opInfo.shortName
                        , style [ ( "width", setLengthForString opInfo.shortName ) ]
                        , id shouldFocusId
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
                                    [ viewExp expression focusTail (breadCrums ++ [ VectorAt index (List.take (index) expressions) (List.drop (index + 1) expressions) ])
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
                                                focusTail
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
                    , id shouldFocusId
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
                        , id shouldFocusId
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
        , viewExp model.expression (model.focus |> List.tail) []
        ]


{-| displays the `view` component, the latex-string and a rendering of the latex-string
-}
viewAll : Model -> Html Msg
viewAll model =
    div [ cls "Container" ]
        [ stylesheet
        , viewExp model.expression (Just model.focus) []
        , pre [] [ latexRepr model.expression |> text ]
        , a [ target "_blank", href <| "https://www.wolframalpha.com/input/?i=" ++ (wolframAlphaText model.expression) ] [ pre [] [ wolframAlphaText model.expression |> text ] ]
        , img [ src <| "https://latex.codecogs.com/svg.latex?%5Cinline%20" ++ (latexRepr model.expression) ] []
        ]


hiddenText string =
    span [ cls "HiddenText" ] [ text string ]


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
