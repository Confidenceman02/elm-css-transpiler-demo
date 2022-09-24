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


type alias ProjectContext =
    { fixPlaceholderModuleKey : Maybe ( Rule.ModuleKey, Range )
    , allCssProperties : Dict String (List (List String))
    }


type alias ModuleContext =
    { moduleName : List String
    , range : Maybe Range
    , isProgramStringTarget : Bool
    , cssStyles : List (List String)
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
        case ( node |> Node.value, direction ) of
            ( Expression.FunctionOrValue moduleName f, Rule.OnEnter ) ->
                let
                    _ =
                        Debug.log "FUNCTION" f

                    isCssModule =
                        List.member moduleName cssModules

                    cssModuleName =
                        if isCssModule then
                            (moduleName ++ [ f ]) :: context.cssStyles

                        else
                            context.cssStyles
                in
                ( [], { context | cssStyles = cssModuleName } )

            _ ->
                ( [], context )


initialProjectContext : ProjectContext
initialProjectContext =
    { fixPlaceholderModuleKey = Nothing, allCssProperties = Dict.empty }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule _ moduleName _ =
    { moduleName = Node.value moduleName
    , range = Nothing
    , isProgramStringTarget = False
    , cssStyles = []
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
            [ ( String.join "." moduleContext.moduleName, moduleContext.cssStyles ) ]
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { fixPlaceholderModuleKey =
        Maybe.Extra.or
            previousContext.fixPlaceholderModuleKey
            newContext.fixPlaceholderModuleKey
    , allCssProperties = Dict.union newContext.allCssProperties previousContext.allCssProperties |> Debug.log "ALL PROPS"
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
