module UnisonLocal.Project exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Project.ProjectSlug as ProjectSlug exposing (ProjectSlug)
import Json.Decode as Decode exposing (nullable)
import Json.Decode.Pipeline exposing (required)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.DateTime as DateTime exposing (DateTime)


type ProjectName
    = ProjectName (Maybe UserHandle) ProjectSlug



-- A Unison Local Project is not the same as a Unison Share Project.
-- There's no reference (instead there's a ProjectName), there's no favs or
-- weekly downloads, summary, or tags.


type alias Project =
    { id : String
    , name : ProjectName
    , defaultBranch : Maybe BranchRef
    , lastActiveBranch : Maybe BranchRef
    , createdAt : DateTime
    , updatedAt : DateTime
    }



-- HELPERS


nameEquals : ProjectName -> ProjectName -> Bool
nameEquals (ProjectName handleA slugA) (ProjectName handleB slugB) =
    let
        handleEquals =
            Maybe.map2 UserHandle.equals handleA handleB
                |> Maybe.withDefault False
    in
    handleEquals && ProjectSlug.equals slugA slugB


nameToApiString : ProjectName -> String
nameToApiString (ProjectName handle slug) =
    let
        handle_ =
            case handle of
                Just h ->
                    UserHandle.toString h ++ "%2F"

                Nothing ->
                    ""
    in
    handle_ ++ ProjectSlug.toString slug



-- DECODE


decode : Decode.Decoder Project
decode =
    let
        makeProject id handle_ slug_ defaultBranch lastActiveBranch createdAt updatedAt =
            let
                name =
                    ProjectName handle_ slug_
            in
            { id = id
            , name = name
            , defaultBranch = defaultBranch
            , lastActiveBranch = lastActiveBranch
            , createdAt = createdAt
            , updatedAt = updatedAt
            }
    in
    Decode.succeed makeProject
        |> required "id" Decode.string
        |> required "userHandle" (nullable UserHandle.decode)
        |> required "slug" ProjectSlug.decode
        |> required "defaultBranch" (nullable BranchRef.decode)
        |> required "lastActiveBranch" (nullable BranchRef.decode)
        |> required "createdAt" DateTime.decode
        |> required "updatedAt" DateTime.decode
