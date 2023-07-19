module UnisonLocal.CodeBrowsingContext exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import UnisonLocal.Project as Project exposing (ProjectName)


type CodeBrowsingContext
    = NonProjectCode
    | ProjectBranch ProjectName BranchRef


nonProjectCode : CodeBrowsingContext
nonProjectCode =
    NonProjectCode


projectBranch : ProjectName -> BranchRef -> CodeBrowsingContext
projectBranch =
    ProjectBranch


equals : CodeBrowsingContext -> CodeBrowsingContext -> Bool
equals a b =
    case ( a, b ) of
        ( NonProjectCode, NonProjectCode ) ->
            True

        ( ProjectBranch nameA branchRefA, ProjectBranch nameB branchRefB ) ->
            Project.nameEquals nameA nameB && BranchRef.equals branchRefA branchRefB

        _ ->
            False
