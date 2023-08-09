module UnisonLocal.Page.ProjectBranchPage exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Html exposing (div, text)
import Html.Attributes exposing (class)
import UI.Icon as Icon
import UI.PageLayout as PageLayout exposing (PageFooter(..))
import UnisonLocal.AppContext exposing (AppContext)
import UnisonLocal.AppDocument as AppDocument exposing (AppDocument)
import UnisonLocal.AppHeader as AppHeader
import UnisonLocal.CodeBrowsingContext as CodeBrowsingContext
import UnisonLocal.Page.CodePage as CodePage
import UnisonLocal.ProjectName as ProjectName exposing (ProjectName)
import UnisonLocal.Route as Route



-- MODEL


type alias Model =
    { code : CodePage.Model }


init : AppContext -> ProjectName -> BranchRef -> Route.CodeRoute -> ( Model, Cmd Msg )
init appContext projectName branchRef codeRoute =
    let
        context =
            CodeBrowsingContext.projectBranch projectName branchRef

        ( code, cmd ) =
            CodePage.init appContext context codeRoute
    in
    ( { code = code }, Cmd.map CodePageMsg cmd )



-- UPDATE


type Msg
    = CodePageMsg CodePage.Msg


update : AppContext -> ProjectName -> BranchRef -> Route.CodeRoute -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectName branchRef codeRoute msg model =
    case msg of
        CodePageMsg codePageMsg ->
            let
                context =
                    CodeBrowsingContext.projectBranch projectName branchRef

                ( codePage_, codePageCmd ) =
                    CodePage.update appContext context codeRoute codePageMsg model.code
            in
            ( { model | code = codePage_ }
            , Cmd.map CodePageMsg codePageCmd
            )


{-| Pass through to CodePage. Used by App when routes change
-}
updateSubPage : AppContext -> ProjectName -> BranchRef -> Model -> Route.CodeRoute -> ( Model, Cmd Msg )
updateSubPage appContext projectName branchRef model codeRoute =
    let
        codeBrowsingContext =
            CodeBrowsingContext.projectBranch projectName branchRef

        ( codePage, codePageCmd ) =
            CodePage.updateSubPage appContext codeBrowsingContext codeRoute model.code
    in
    ( { model | code = codePage }
    , Cmd.map CodePageMsg codePageCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map CodePageMsg (CodePage.subscriptions model.code)



-- VIEW


view : AppContext -> ProjectName -> BranchRef -> Model -> AppDocument Msg
view appContext projectName branchRef model =
    let
        appHeader =
            AppHeader.appHeader
                |> AppHeader.withLeftSide
                    [ div [ class "item" ] [ Icon.view Icon.pencilRuler, text (ProjectName.toString projectName) ]
                    , div [ class "item" ] [ Icon.view Icon.branch, text (BranchRef.toString branchRef) ]
                    ]

        ( codePage_, modal_ ) =
            CodePage.view appContext
                CodePageMsg
                (CodeBrowsingContext.projectBranch projectName branchRef)
                model.code
    in
    AppDocument.appDocument
        "project-branch-page"
        "Project"
        appHeader
        (PageLayout.view codePage_)
        |> AppDocument.withModal_ modal_
