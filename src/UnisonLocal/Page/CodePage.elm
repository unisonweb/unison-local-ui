module UnisonLocal.Page.CodePage exposing (..)

import Code.CodebaseTree as CodebaseTree
import Code.Config exposing (Config)
import Code.Definition.Reference exposing (Reference)
import Code.Finder as Finder
import Code.Finder.SearchOptions as SearchOptions
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Namespace exposing (NamespaceDetails)
import Code.Perspective as Perspective exposing (Perspective)
import Code.ReadmeCard as ReadmeCard
import Code.Workspace as Workspace
import Html exposing (Html)
import Lib.HttpApi as HttpApi
import RemoteData exposing (WebData)
import UI.KeyboardShortcut as KeyboardShortcut
import UI.KeyboardShortcut.Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.Sidebar as Sidebar exposing (Sidebar)
import UI.ViewMode as ViewMode exposing (ViewMode)
import UnisonLocal.CodeBrowsingContext exposing (CodeBrowsingContext(..))
import UnisonLocal.Env as Env exposing (Env)
import UnisonLocal.Page.CodePageContent as CodePageContent
import UnisonLocal.Route as Route exposing (CodeRoute(..))



-- MODEL


type PageModal
    = NoModal
    | FinderModal Finder.Model


type CodeContent
    = PerspectivePage ReadmeCard.Model
    | WorkspacePage Workspace.Model


type alias Model =
    { content : CodeContent
    , sidebarToggled : Bool
    , codebaseTree : CodebaseTree.Model
    , config : Config
    , modal : PageModal
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : Env -> CodeBrowsingContext -> CodeRoute -> ( Model, Cmd Msg )
init env context codeRoute =
    let
        config =
            Env.toCodeConfig env context perspective

        perspective =
            case codeRoute of
                CodeRoot p ->
                    Perspective.fromParams p

                Definition p _ ->
                    Perspective.fromParams p

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init config

        ( content, cmd ) =
            case codeRoute of
                CodeRoot _ ->
                    ( PerspectivePage ReadmeCard.init
                    , Cmd.none
                    )

                Definition _ ref ->
                    let
                        ( workspace, workspaceCmd ) =
                            Workspace.init config (Just ref)
                    in
                    ( WorkspacePage workspace
                    , Cmd.map WorkspaceMsg workspaceCmd
                    )

        fetchNamespaceDetailsCmd =
            config.perspective
                |> CodePageContent.fetchNamespaceDetails
                    FetchPerspectiveNamespaceDetailsFinished
                    context
                |> Maybe.map (HttpApi.perform env.api)
                |> Maybe.withDefault Cmd.none
    in
    ( { content = content
      , sidebarToggled = False
      , codebaseTree = codebaseTree
      , config = config
      , modal = NoModal
      , keyboardShortcut = KeyboardShortcut.init env.operatingSystem
      }
    , Cmd.batch
        [ cmd
        , Cmd.map CodebaseTreeMsg codebaseTreeCmd
        , fetchNamespaceDetailsCmd
        ]
    )



-- UPDATE


type Msg
    = ShowFinderModal
    | CloseModal
    | UpOneLevel
    | ChangePerspectiveToNamespace FQN
    | ToggleSidebar
    | FetchPerspectiveNamespaceDetailsFinished FQN (WebData NamespaceDetails)
    | ReadmeCardMsg ReadmeCard.Msg
    | Keydown KeyboardEvent
    | FinderMsg Finder.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg
    | CodebaseTreeMsg CodebaseTree.Msg
    | WorkspaceMsg Workspace.Msg


update : Env -> CodeBrowsingContext -> ViewMode -> CodeRoute -> Msg -> Model -> ( Model, Cmd Msg )
update env context viewMode codeRoute msg model_ =
    let
        -- Always update the subPage since url/route changes often happens out
        -- of band.
        -- TODO: When is this ever needed outside of Route changes from App?,
        -- like when do we need it via `update`? its supposed to be out of band
        -- right?
        ( model, cmd ) =
            updateSubPage env context codeRoute model_
    in
    case ( model.content, msg ) of
        ( _, ShowFinderModal ) ->
            let
                ( fm, fCmd ) =
                    Finder.init model.config (SearchOptions.init model.config.perspective Nothing)
            in
            ( { model | modal = FinderModal fm }, Cmd.batch [ cmd, Cmd.map FinderMsg fCmd ] )

        ( _, CloseModal ) ->
            ( { model | modal = NoModal }, cmd )

        ( _, FetchPerspectiveNamespaceDetailsFinished fqn details ) ->
            let
                config =
                    model.config

                perspective =
                    case model.config.perspective of
                        Perspective.Root r ->
                            if FQN.isRoot fqn then
                                Perspective.Root { r | details = details }

                            else
                                config.perspective

                        Perspective.Namespace p ->
                            if FQN.equals p.fqn fqn then
                                Perspective.Namespace { p | details = details }

                            else
                                config.perspective

                nextConfig =
                    { config | perspective = perspective }
            in
            ( { model | config = nextConfig }, cmd )

        ( _, UpOneLevel ) ->
            let
                newPerspective =
                    Perspective.upOneLevel model.config.perspective

                navCmd =
                    navigateToCode env context (Route.replacePerspective (routeReference codeRoute) newPerspective)
            in
            ( model, Cmd.batch [ cmd, navCmd ] )

        ( _, ChangePerspectiveToNamespace fqn ) ->
            let
                perspective =
                    Perspective.toNamespacePerspective model.config.perspective fqn

                navCmd =
                    navigateToCode env context (Route.replacePerspective (routeReference codeRoute) perspective)
            in
            ( model, Cmd.batch [ cmd, navCmd ] )

        ( _, ToggleSidebar ) ->
            ( { model | sidebarToggled = not model.sidebarToggled }, cmd )

        ( _, CodebaseTreeMsg codebaseTreeMsg ) ->
            let
                ( codebaseTree, codebaseTreeCmd, outMsg ) =
                    CodebaseTree.update model.config codebaseTreeMsg model.codebaseTree

                ( m, cmd_ ) =
                    ( { model | codebaseTree = codebaseTree }
                    , Cmd.map CodebaseTreeMsg codebaseTreeCmd
                    )
            in
            case outMsg of
                CodebaseTree.None ->
                    ( m, cmd_ )

                CodebaseTree.OpenDefinition ref ->
                    let
                        navCmd =
                            navigateToCode env context (Route.definition model.config.perspective ref)

                        -- Close the sidebar when opening items on mobile
                        m_ =
                            if m.sidebarToggled then
                                { m | sidebarToggled = False }

                            else
                                m
                    in
                    ( m_, Cmd.batch [ cmd, cmd_, navCmd ] )

                CodebaseTree.ChangePerspectiveToNamespace fqn ->
                    let
                        perspective =
                            Perspective.toNamespacePerspective model.config.perspective fqn

                        ref =
                            case codeRoute of
                                Definition _ r ->
                                    Just r

                                _ ->
                                    Nothing

                        navCmd =
                            navigateToCode env context (Route.replacePerspective ref perspective)
                    in
                    ( m, Cmd.batch [ cmd, cmd_, navCmd ] )

        ( _, FinderMsg finderMsg ) ->
            case model.modal of
                FinderModal fm ->
                    let
                        ( fm_, fCmd, outMsg ) =
                            Finder.update model.config finderMsg fm
                    in
                    case outMsg of
                        Finder.Remain ->
                            ( { model | modal = FinderModal fm_ }, Cmd.batch [ cmd, Cmd.map FinderMsg fCmd ] )

                        Finder.Exit ->
                            ( { model | modal = NoModal }, Cmd.batch [ cmd, Cmd.map FinderMsg fCmd ] )

                        Finder.OpenDefinition r ->
                            ( { model | modal = NoModal }
                            , Cmd.batch
                                [ cmd
                                , Cmd.map FinderMsg fCmd
                                , navigateToCode env context (Route.definition model.config.perspective r)
                                ]
                            )

                _ ->
                    ( model, cmd )

        ( _, Keydown event ) ->
            -- When handling keydown, we don't want to run updateSubPage
            -- Since this event is handled by Workspace as well, and if both are
            -- handling it with updateSubPage in play, a closed definition (by
            -- hitting 'x' on the keyboard) is re-opened as the next event
            -- happens before the route change to the newly focused item and thus
            -- the old item is re-opened.
            keydown env model_ event

        ( _, KeyboardShortcutMsg kMsg ) ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.batch [ cmd, Cmd.map KeyboardShortcutMsg kCmd ] )

        ( PerspectivePage rm, ReadmeCardMsg readmeCardMsg ) ->
            let
                ( readmeCard, rmCmd, out ) =
                    ReadmeCard.update model.config readmeCardMsg rm

                navCmd =
                    case out of
                        ReadmeCard.OpenDefinition r ->
                            navigateToCode env context (Route.definition model.config.perspective r)

                        _ ->
                            Cmd.none
            in
            ( { model | content = PerspectivePage readmeCard }
            , Cmd.batch [ cmd, navCmd, Cmd.map ReadmeCardMsg rmCmd ]
            )

        ( WorkspacePage workspace, WorkspaceMsg workspaceMsg ) ->
            let
                ( workspace_, workspaceCmd, outMsg ) =
                    Workspace.update model.config viewMode workspaceMsg workspace

                ( m, outCmd ) =
                    case outMsg of
                        Workspace.Focused ref ->
                            ( model, navigateToCode env context (Route.definition model.config.perspective ref) )

                        Workspace.Emptied ->
                            ( model, navigateToCode env context (Route.codeRoot model.config.perspective) )

                        Workspace.ChangePerspectiveToSubNamespace ref subFqn ->
                            let
                                perspective =
                                    let
                                        fullFqn =
                                            case model.config.perspective of
                                                Perspective.Namespace { fqn } ->
                                                    FQN.append fqn subFqn

                                                _ ->
                                                    subFqn
                                    in
                                    Perspective.toNamespacePerspective model.config.perspective fullFqn
                            in
                            ( model, navigateToCode env context (Route.replacePerspective ref perspective) )

                        Workspace.ShowFinderRequest adhocFqn ->
                            let
                                ( fm, fCmd ) =
                                    Finder.init model.config (SearchOptions.init model.config.perspective (Just adhocFqn))
                            in
                            ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fCmd )

                        _ ->
                            ( model, Cmd.none )
            in
            ( { m | content = WorkspacePage workspace_ }
            , Cmd.batch [ cmd, outCmd, Cmd.map WorkspaceMsg workspaceCmd ]
            )

        _ ->
            ( model, cmd )


updateSubPage : Env -> CodeBrowsingContext -> CodeRoute -> Model -> ( Model, Cmd Msg )
updateSubPage env codeBrowsingContext codeRoute model =
    let
        toConfig =
            Env.toCodeConfig env codeBrowsingContext

        refreshSidebar newConfig m =
            CodePageContent.fetchPerspectiveAndCodebaseTree
                env
                newConfig
                FetchPerspectiveNamespaceDetailsFinished
                CodebaseTreeMsg
                codeBrowsingContext
                model.config.perspective
                m

        -- Don't replace the perspective (it has namespace details state) when the page refreshes.
        config p =
            if Perspective.equals p model.config.perspective then
                model.config

            else
                toConfig p
    in
    case codeRoute of
        CodeRoot p ->
            let
                persp =
                    p |> Perspective.fromParams

                config_ =
                    config persp

                ( model2, cmd ) =
                    refreshSidebar config_ model

                content =
                    case model.content of
                        PerspectivePage rc ->
                            PerspectivePage rc

                        _ ->
                            PerspectivePage ReadmeCard.init
            in
            ( { model2 | config = config_, content = content }, cmd )

        Definition p ref ->
            let
                persp =
                    p |> Perspective.fromParams

                config_ =
                    config persp

                ( workspace, workspaceCmd ) =
                    case model.content of
                        WorkspacePage ws ->
                            Workspace.open config_ ws ref

                        _ ->
                            Workspace.init config_ (Just ref)

                model2 =
                    { model | config = config_, content = WorkspacePage workspace }

                ( model3, cmd ) =
                    refreshSidebar config_ model2
            in
            ( model3
            , Cmd.batch [ Cmd.map WorkspaceMsg workspaceCmd, cmd ]
            )


routeReference : CodeRoute -> Maybe Reference
routeReference route =
    case route of
        Definition _ r ->
            Just r

        _ ->
            Nothing


keydown : Env -> Model -> KeyboardEvent -> ( Model, Cmd Msg )
keydown env model keyboardEvent =
    let
        shortcut =
            KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut keyboardEvent

        noOp =
            ( model, Cmd.none )

        toggleSidebar =
            ( { model | sidebarToggled = not model.sidebarToggled }, Cmd.none )
    in
    case shortcut of
        KeyboardShortcut.Chord Ctrl (B _) ->
            toggleSidebar

        KeyboardShortcut.Chord Meta (B _) ->
            toggleSidebar

        KeyboardShortcut.Sequence _ Escape ->
            ( { model | modal = NoModal }, Cmd.none )

        _ ->
            if Finder.isShowFinderKeyboardShortcut env.operatingSystem shortcut then
                let
                    ( finder, cmd ) =
                        Finder.init model.config
                            (SearchOptions.init model.config.perspective Nothing)
                in
                ( { model | modal = FinderModal finder }, Cmd.map FinderMsg cmd )

            else
                noOp



-- EFFECTS


navigateToCode : Env -> CodeBrowsingContext -> CodeRoute -> Cmd Msg
navigateToCode env context codeRoute =
    let
        route_ =
            case context of
                NonProjectCode ->
                    Route.nonProjectCode codeRoute

                ProjectBranch ps bs ->
                    Route.projectBranch ps bs codeRoute
    in
    Route.navigate env.navKey route_



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.content of
        PerspectivePage _ ->
            KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown

        WorkspacePage ws ->
            Sub.batch
                [ KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown
                , Sub.map WorkspaceMsg (Workspace.subscriptions ws)
                ]



-- VIEW


viewContent : ViewMode -> Perspective -> CodeContent -> PageContent Msg
viewContent viewMode perspective content =
    case content of
        PerspectivePage readmeCard ->
            PageContent.oneColumn
                (CodePageContent.viewPerspectiveLandingPage
                    ReadmeCardMsg
                    ShowFinderModal
                    perspective
                    readmeCard
                )

        WorkspacePage workspace ->
            PageContent.oneColumn [ Html.map WorkspaceMsg (Workspace.view viewMode workspace) ]


viewSidebar : Model -> Sidebar Msg
viewSidebar model =
    let
        codebaseTree =
            Just { codebaseTree = model.codebaseTree, codebaseTreeMsg = CodebaseTreeMsg }
    in
    CodePageContent.viewSidebar
        model.config.perspective
        { upOneLevelMsg = UpOneLevel
        , showFinderModalMsg = ShowFinderModal
        , changePerspectiveToNamespaceMsg = ChangePerspectiveToNamespace
        }
        codebaseTree
        |> Sidebar.withToggle
            { isToggled = model.sidebarToggled, toggleMsg = ToggleSidebar }


view : Env -> (Msg -> msg) -> ViewMode -> CodeBrowsingContext -> Model -> ( PageLayout msg, Maybe (Html msg) )
view env toMsg viewMode context model =
    let
        content =
            PageContent.map toMsg (viewContent viewMode model.config.perspective model.content)

        modal =
            case model.modal of
                NoModal ->
                    Nothing

                FinderModal fm ->
                    Just (Html.map toMsg (Html.map FinderMsg (Finder.view fm)))
    in
    case ( model.content, viewMode ) of
        ( PerspectivePage _, ViewMode.Regular ) ->
            ( PageLayout.sidebarLeftContentLayout
                env.operatingSystem
                (Sidebar.map toMsg (viewSidebar model))
                content
                (PageLayout.PageFooter [])
                |> PageLayout.withSidebarToggle model.sidebarToggled
            , modal
            )

        ( WorkspacePage _, ViewMode.Regular ) ->
            ( PageLayout.sidebarLeftContentLayout
                env.operatingSystem
                (Sidebar.map toMsg (viewSidebar model))
                content
                (PageLayout.PageFooter [])
                |> PageLayout.withSidebarToggle model.sidebarToggled
                |> PageLayout.withSubduedBackground
            , modal
            )

        ( _, ViewMode.Presentation ) ->
            ( PageLayout.PresentationLayout content, modal )
