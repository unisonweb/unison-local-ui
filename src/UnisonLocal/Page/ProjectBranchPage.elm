module UnisonLocal.Page.ProjectBranchPage exposing (..)

import Code.BranchRef exposing (BranchRef)
import Html exposing (text)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageFooter(..))
import UnisonLocal.AppDocument as AppDocument exposing (AppDocument)
import UnisonLocal.AppHeader as AppHeader
import UnisonLocal.CodeBrowsingContext as CodeBrowsingContext
import UnisonLocal.Env exposing (Env)
import UnisonLocal.Page.CodePage as CodePage
import UnisonLocal.Project exposing (ProjectName)
import UnisonLocal.Route as Route



-- MODEL


type alias Model =
    { code : CodePage.Model }


init : Env -> ProjectName -> BranchRef -> Route.CodeRoute -> ( Model, Cmd Msg )
init env projectName branchRef codeRoute =
    let
        context =
            CodeBrowsingContext.projectBranch projectName branchRef

        ( code, cmd ) =
            CodePage.init env context codeRoute
    in
    ( { code = code }, Cmd.map CodePageMsg cmd )



-- UPDATE


type Msg
    = NoOp
    | CodePageMsg CodePage.Msg


update : Env -> ProjectName -> BranchRef -> Route.CodeRoute -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ _ _ model =
    ( model, Cmd.none )


{-| Pass through to CodePage. Used by App when routes change
-}
updateSubPage : Env -> ProjectName -> BranchRef -> Model -> Route.CodeRoute -> ( Model, Cmd Msg )
updateSubPage env projectName branchRef model codeRoute =
    let
        codeBrowsingContext =
            CodeBrowsingContext.projectBranch projectName branchRef

        ( codePage, codePageCmd ) =
            CodePage.updateSubPage env codeBrowsingContext codeRoute model.code
    in
    ( { model | code = codePage }
    , Cmd.map CodePageMsg codePageCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : ProjectName -> BranchRef -> Model -> AppDocument Msg
view _ _ _ =
    let
        appHeader =
            AppHeader.appHeader

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn [ text "project branch" ])
                (PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    AppDocument.appDocument
        "project-branch-page"
        "TODO Project Branch"
        appHeader
        (PageLayout.view page)
