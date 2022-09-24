module ExtractCss exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Maybe.Extra
import Review.Fix
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


type alias ProjectContext =
    { fixPlaceholderModuleKey : Maybe ( Rule.ModuleKey, Range )
    }


type alias ModuleContext =
    { range : Maybe Range
    , isSpecialModule : Bool
    }


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
expressionVisitor node _ context =
    if context.isSpecialModule then
        case node |> Node.value of
            Expression.Literal literalString ->
                if literalString == "<replacement-placeholder>" then
                    ( [], { context | range = Just (Node.range node) } )

                else
                    ( [], context )

            _ ->
                ( [], context )

    else
        ( [], context )


initialProjectContext : ProjectContext
initialProjectContext =
    { fixPlaceholderModuleKey = Nothing }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> ProjectContext -> ModuleContext
fromProjectToModule moduleKey moduleName projectContext =
    { range = Nothing
    , isSpecialModule = False
    }


fromModuleToProject : Rule.ModuleKey -> Node ModuleName -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey moduleName moduleContext =
    { fixPlaceholderModuleKey =
        if Node.value moduleName == [ "StubCssGenerator" ] then
            moduleContext.range
                |> Maybe.map (Tuple.pair moduleKey)

        else
            Nothing
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { fixPlaceholderModuleKey =
        Maybe.Extra.or
            previousContext.fixPlaceholderModuleKey
            newContext.fixPlaceholderModuleKey
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
            | isSpecialModule = True
          }
        )

    else
        ( [], context )
