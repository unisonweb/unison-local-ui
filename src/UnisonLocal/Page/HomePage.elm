module UnisonLocal.Page.HomePage exposing (..)

import Code.BranchRef as BranchRef exposing (BranchSlug(..))
import Code.Perspective as Perspective
import Html exposing (li, p, text, ul)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Lib.HttpApi as HttpApi
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI.Click as Click
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageFooter(..))
import UI.PageTitle as PageTitle
import UnisonLocal.Api as LocalApi
import UnisonLocal.AppContext exposing (AppContext)
import UnisonLocal.AppDocument as AppDocument exposing (AppDocument)
import UnisonLocal.AppHeader as AppHeader
import UnisonLocal.ProjectName as ProjectName exposing (ProjectName)
import UnisonLocal.Route as Route


type alias Model =
    { projects : List ProjectWithBranches }


type alias ProjectWithBranches =
    { projectName : ProjectName
    , branches : List { branchName : BranchSlug }
    }


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    let
        fetchProjectsCmd =
            fetchProjects FetchProjectsFinished
                |> HttpApi.perform appContext.api
    in
    ( { projects = [] }
    , fetchProjectsCmd
    )


fetchProjects :
    (WebData (List { projectName : ProjectName }) -> msg)
    -> HttpApi.ApiRequest (List { projectName : ProjectName }) msg
fetchProjects finishedMsg =
    LocalApi.projects
        |> HttpApi.toRequest decodeProjectList (RemoteData.fromResult >> finishedMsg)


fetchProjectBranches :
    (WebData (List { branchName : BranchSlug }) -> msg)
    -> ProjectName
    -> HttpApi.ApiRequest (List { branchName : BranchSlug }) msg
fetchProjectBranches finishedMsg projectName =
    LocalApi.projectBranches projectName
        |> HttpApi.toRequest decodeBranchList (RemoteData.fromResult >> finishedMsg)


decodeProjectList : Decode.Decoder (List { projectName : ProjectName })
decodeProjectList =
    let
        makeProjectName projectName =
            { projectName = projectName }
    in
    Decode.succeed makeProjectName
        |> required "projectName" ProjectName.decode
        |> Decode.list


decodeBranchList : Decode.Decoder (List { branchName : BranchSlug })
decodeBranchList =
    let
        makeBranchName branchName =
            { branchName = branchName }
    in
    Decode.succeed makeBranchName
        |> required "branchName" branchSlugDecode
        |> Decode.list


branchSlugDecode : Decode.Decoder BranchSlug
branchSlugDecode =
    Decode.map BranchRef.branchSlugFromString Decode.string
        |> Decode.andThen (Util.decodeFailInvalid "Invalid BranchName")


type Msg
    = FetchProjectsFinished (WebData (List { projectName : ProjectName }))
    | FetchProjectBranchesFinished ProjectName (WebData (List { branchName : BranchSlug }))


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update appContext msg model =
    case msg of
        FetchProjectsFinished (Success projects) ->
            ( { projects =
                    projects
                        |> List.map (\{ projectName } -> ProjectWithBranches projectName [])
              }
            , let
                fetchProjectBranchesCmd projectName =
                    fetchProjectBranches (FetchProjectBranchesFinished projectName) projectName
                        |> HttpApi.perform appContext.api
              in
              projects
                |> List.map (\{ projectName } -> fetchProjectBranchesCmd projectName)
                |> Cmd.batch
            )

        FetchProjectsFinished _ ->
            ( model, Cmd.none )

        FetchProjectBranchesFinished projectName (Success branches) ->
            ( { projects =
                    model.projects
                        |> List.map
                            (\project ->
                                if project.projectName == projectName then
                                    ProjectWithBranches project.projectName branches

                                else
                                    project
                            )
              }
            , Cmd.none
            )

        FetchProjectBranchesFinished _ _ ->
            ( model, Cmd.none )


view : Model -> AppDocument Msg
view { projects } =
    let
        appHeader =
            AppHeader.appHeader

        projectList =
            projects
                |> List.map
                    (\{ projectName, branches } ->
                        let
                            defaultBranch =
                                List.head branches
                                    |> Maybe.map (\{ branchName } -> branchName)
                                    |> Maybe.withDefault (BranchSlug "main")
                        in
                        li []
                            [ Click.href
                                (String.join "/" [ "/projects", ProjectName.toString projectName, BranchRef.branchSlugToString defaultBranch ++ "/" ])
                                |> Click.view [] [ text <| ProjectName.toString projectName ]
                            ]
                    )

        nonProjectCodeParagraph =
            p []
                [ text "or "
                , Route.nonProjectCodeRoot Perspective.relativeRootPerspective
                    |> Route.toUrlString
                    |> Click.href
                    |> Click.view [] [ text "view all non-project code" ]
                , text "."
                ]

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn
                    [ ul [] projectList
                    , nonProjectCodeParagraph
                    ]
                    |> PageContent.withPageTitle (PageTitle.title "Open a project")
                )
                (PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    AppDocument.appDocument
        "home-page"
        "Explore your codebase"
        appHeader
        (PageLayout.view page)
