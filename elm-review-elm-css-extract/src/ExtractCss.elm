module ExtractCss exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Maybe.Extra
import Review.Fix
import Review.Rule as Rule exposing (Rule)
import String exposing (fromInt)


type alias Zipper a =
    ( List a, a, List a )


type alias ProjectContext =
    { fixPlaceholderModuleKey : Maybe ( Rule.ModuleKey, Range )
    , allCssProperties : Dict String (List (List String))
    }


type alias ModuleContext =
    { moduleName : List String
    , range : Maybe Range
    , isProgramStringTarget : Bool
    , cssStyles : Dict Int (List (List String))
    , current : Maybe ( Int, Zipper (Node Expression) )
    }


cssModules : List (List String)
cssModules =
    [ [ "Html", "Styled", "Attributes" ], [ "Css" ] ]


rule : Rule
rule =
    Rule.newProjectRuleSchema "CssReplacement" initialProjectContext
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluationForProject
        |> Rule.fromProjectRuleSchema


moduleVisitor :
    Rule.ModuleRuleSchema {} ModuleContext
    -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withExpressionVisitor expressionVisitor


expressionVisitor : Node Expression -> Rule.Direction -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node direction context =
    if direction == Rule.OnEnter then
        if context.isProgramStringTarget then
            case node |> Node.value of
                Expression.Literal literalString ->
                    if literalString == "<replacement-placeholder>" then
                        ( [], { context | range = Just (Node.range node) } )

                    else
                        ( [], context )

                _ ->
                    ( [], context )

        else
            case node |> Node.value of
                -- Css attributes only exist in lists, so lists are all we care about.
                Expression.ListExpr nodes ->
                    let
                        fn : Zipper (Node Expression) -> List (List String) -> List (List String)
                        fn zipper styles =
                            case zipper of
                                -- When only 1 element in list or when last element
                                ( _, pt, [] ) ->
                                    styles ++ getStyles pt

                                -- Resolve styles and walk the zipper
                                ( ln, pt, rh :: rn ) ->
                                    fn ( pt :: ln, rh, rn ) (styles ++ getStyles pt)
                    in
                    case context.current of
                        -- First element in a zipper with > 1 element
                        Just ( key, ( [], pt, rh :: rn ) ) ->
                            ( []
                            , { context
                                | current =
                                    Just ( key, ( [ pt ], rh, rn ) )
                              }
                            )

                        -- Walk the zipper
                        Just ( key, ( ls, pt, rh :: rs ) ) ->
                            ( []
                            , { context
                                | current = Just ( key, ( pt :: ls, rh, rs ) )
                              }
                            )

                        -- We have reached the end of the list, set to Nothing so we are ready for the next List
                        Just _ ->
                            ( []
                            , { context
                                | current = Nothing
                              }
                            )

                        _ ->
                            -- We have visited a ListExpr, if it has styles, lets get them all on the first pass.
                            case toZipper nodes of
                                Just zipper ->
                                    let
                                        key =
                                            Dict.values context.cssStyles
                                                |> List.length
                                    in
                                    ( []
                                    , { context
                                        | current =
                                            Just
                                                ( key
                                                , zipper
                                                )
                                        , cssStyles =
                                            Dict.insert key
                                                (fn zipper [])
                                                context.cssStyles
                                      }
                                    )

                                -- The ListExpr is empty, do nothing.
                                _ ->
                                    ( [], context )

                -- Expression.FunctionOrValue moduleName f ->
                --     let
                --         -- _ =
                --         --     Debug.log "FUNCTION" f
                --         isCssModule =
                --             List.member moduleName cssModules
                --         cssModuleName =
                --             if isCssModule then
                --                 (moduleName ++ [ f ]) :: context.cssStyles
                --             else
                --                 context.cssStyles
                --     in
                --     ( [], { context | cssStyles = cssModuleName } )
                _ ->
                    ( [], context )

    else
        ( [], context )


getStyles : Node Expression -> List (List String)
getStyles expr =
    let
        fn : List (List String) -> List (Node Expression) -> List (List String)
        fn acc nodes =
            case nodes of
                [] ->
                    acc

                head :: rest ->
                    case Node.value head of
                        Expression.FunctionOrValue moduleName f ->
                            fn ((moduleName ++ [ f ]) :: acc) rest

                        Expression.Application n ->
                            fn acc (n ++ rest)

                        Expression.ListExpr n ->
                            fn acc (n ++ rest)

                        Expression.Literal lit ->
                            fn ([ lit ] :: acc) rest

                        Expression.ParenthesizedExpression pe ->
                            fn ([ ")" ] :: fn ([ "(" ] :: acc) [ pe ]) rest

                        _ ->
                            fn acc rest
    in
    fn [] [ expr ]


initialProjectContext : ProjectContext
initialProjectContext =
    { fixPlaceholderModuleKey = Nothing, allCssProperties = Dict.empty }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ moduleName _ =
    { moduleName = Node.value moduleName
    , range = Nothing
    , isProgramStringTarget = False
    , cssStyles = Dict.empty
    , current = Nothing
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName moduleContext =
    { fixPlaceholderModuleKey =
        if Node.value moduleName == [ "StubCssGenerator" ] then
            moduleContext.range
                |> Maybe.map (Tuple.pair moduleKey)

        else
            Nothing
    , allCssProperties =
        Dict.fromList
            [ ( String.join "." moduleContext.moduleName, List.concat <| Dict.values moduleContext.cssStyles ) ]
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { fixPlaceholderModuleKey =
        Maybe.Extra.or
            previousContext.fixPlaceholderModuleKey
            newContext.fixPlaceholderModuleKey
    , allCssProperties = Dict.union newContext.allCssProperties previousContext.allCssProperties
    }


finalEvaluationForProject : ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluationForProject projectContext =
    case projectContext.fixPlaceholderModuleKey of
        Just ( moduleKey, range ) ->
            [ Rule.errorForModuleWithFix moduleKey
                { message = "TODO"
                , details = [ "" ]
                }
                range
                [ Review.Fix.replaceRangeBy range """"import Css\\n\\nclasses = [ [ Css.backgroundColor (Css.hex \\"#ff375a\\"), Css.color (Css.hex \\"#ffffff\\") ] ]"
"""
                ]
            ]

        _ ->
            []


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
moduleDefinitionVisitor node context =
    if (Node.value node |> Module.moduleName) == [ "StubCssGenerator" ] then
        ( []
        , { context
            | isProgramStringTarget = True
            , moduleName = Node.value node |> Module.moduleName
          }
        )

    else
        ( [], { context | isProgramStringTarget = False } )


{-| The zipper gives us co-monad power. We can aslo iterate forwards and backwards in constant time.

The nice thing about this is that for properties that require an extra expression to be valid, we can look ahead
and check what the value is.

e.g. The expression "Css.color" requires another expression to be valid i.e. "#ffffff".

In zipper structure we would see

                  ( \_, FunctionOrValue Css.Color, Literal "#ffffff" )
                        ^^^^^^^<CURRENT>^^^^^^^^^  ^^^^^^NEXT^^^^^^^

-}
toZipper : List a -> Maybe (Zipper a)
toZipper =
    uncons (\() -> Nothing) (\head rest -> Just ( [], head, rest ))


uncons : (() -> b) -> (a -> List a -> b) -> List a -> b
uncons f g list =
    case list of
        [] ->
            f ()

        x :: xs ->
            g x xs
