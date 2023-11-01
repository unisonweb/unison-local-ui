module UnisonLocal.Page.HomePage exposing (..)

import Code.BranchRef as BranchRef exposing (BranchSlug(..))
import Code.Perspective as Perspective
import Dict exposing (Dict)
import Html exposing (Html, div, h2, p, text)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI.Click as Click
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageFooter(..))
import UI.PageTitle as PageTitle
import UI.Tag as Tag
import UnisonLocal.Api as LocalApi
import UnisonLocal.AppContext exposing (AppContext)
import UnisonLocal.AppDocument as AppDocument exposing (AppDocument)
import UnisonLocal.AppHeader as AppHeader
import UnisonLocal.ProjectName as ProjectName exposing (ProjectName)
import UnisonLocal.Route as Route



-- MODEL


type alias Model =
    { projects : Projects }


type alias Projects =
    -- Since a `Dict` requires a key of type `comparable`
    -- `ProjectName` is made available in the value
    -- for further processing
    Dict String ( ProjectName, List BranchSlug )


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    ( { projects = Dict.empty }
    , fetchProjects
        |> HttpApi.perform appContext.api
    )



-- UPDATE


type Msg
    = FetchProjectsFinished (WebData (List ProjectName))
    | FetchProjectBranchesFinished (WebData ( ProjectName, List BranchSlug ))


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update appContext msg model =
    case msg of
        FetchProjectsFinished (Success projectNames) ->
            ( { projects =
                    projectNames
                        |> List.map
                            (\p ->
                                ( ProjectName.toString p
                                , ( p, [] )
                                )
                            )
                        |> Dict.fromList
              }
            , projectNames
                |> List.map
                    (fetchProjectBranches
                        >> HttpApi.perform appContext.api
                    )
                |> Cmd.batch
            )

        FetchProjectBranchesFinished (Success ( projectName, branches )) ->
            ( { model
                | projects =
                    model.projects
                        |> Dict.insert
                            (ProjectName.toString projectName)
                            ( projectName, branches )
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- EFFECTS


fetchProjects : HttpApi.ApiRequest (List ProjectName) Msg
fetchProjects =
    LocalApi.projects
        |> HttpApi.toRequest decodeProjectList (RemoteData.fromResult >> FetchProjectsFinished)


fetchProjectBranches :
    ProjectName
    -> HttpApi.ApiRequest ( ProjectName, List BranchSlug ) Msg
fetchProjectBranches projectName =
    let
        decodeWithProjectName =
            decodeBranchList
                |> Decode.map (Tuple.pair projectName)
    in
    LocalApi.projectBranches projectName
        |> HttpApi.toRequest decodeWithProjectName (RemoteData.fromResult >> FetchProjectBranchesFinished)



-- DECODE


decodeProjectList : Decode.Decoder (List ProjectName)
decodeProjectList =
    Decode.list <|
        Decode.field "projectName" ProjectName.decode


decodeBranchList : Decode.Decoder (List BranchSlug)
decodeBranchList =
    let
        branchSlugDecode =
            Decode.map BranchRef.branchSlugFromString Decode.string
                |> Decode.andThen (Util.decodeFailInvalid "Invalid BranchName")
    in
    Decode.list <|
        Decode.field "branchName" branchSlugDecode



-- VIEW


viewProjectList : Projects -> List (Html Msg)
viewProjectList projects =
    let
        branchTag projectName branchName =
            BranchRef.projectBranchRef branchName
                |> (\branchRef ->
                        BranchRef.toTag branchRef
                            |> Tag.withClick
                                (Route.projectBranchRoot projectName branchRef Perspective.relativeRootPerspective
                                    |> Route.toUrlString
                                    |> Click.href
                                )
                   )
                |> Tag.view

        branchList projectName branches =
            case branches of
                [] ->
                    [ text "No branches" ]

                branchNames ->
                    branchNames
                        |> List.map (branchTag projectName)
                        |> List.intersperse (text " ")

        projectItem projectName branches =
            div []
                [ h2 [] [ text <| ProjectName.toString projectName ]
                , p [] (branchList projectName branches)
                ]
    in
    projects
        |> Dict.toList
        |> List.map
            (\( _, ( projectName, branches ) ) ->
                projectItem projectName branches
            )


view : Model -> AppDocument Msg
view { projects } =
    let
        appHeader =
            AppHeader.appHeader

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
                    (viewProjectList projects
                        ++ [ nonProjectCodeParagraph ]
                    )
                    |> PageContent.withPageTitle (PageTitle.title "Open a project branch")
                )
                (PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    AppDocument.appDocument
        "home-page"
        "Explore your codebase"
        appHeader
        (PageLayout.view page)
