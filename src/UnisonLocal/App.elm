module UnisonLocal.App exposing (..)

import Browser
import Browser.Navigation as Nav
import Code.CodebaseTree as CodebaseTree
import Code.Definition.Reference exposing (Reference)
import Code.Finder as Finder
import Code.Finder.SearchOptions as SearchOptions
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hashvatar as Hashvatar
import Code.Namespace as Namespace exposing (NamespaceDetails)
import Code.Perspective as Perspective exposing (Perspective(..))
import Code.Workspace as Workspace
import Code.Workspace.WorkspaceItems as WorkspaceItems
import Html exposing (Html, div, h1, h2, h3, p, section, span, strong, text)
import Html.Attributes exposing (class, classList, id, title)
import Http
import Lib.HttpApi as HttpApi
import Lib.OperatingSystem exposing (OperatingSystem(..))
import Lib.Util as Util
import RemoteData
import UI
import UI.ActionMenu as ActionMenu
import UI.AppHeader as AppHeader
import UI.Button as Button
import UI.Click as Click exposing (Click)
import UI.CopyField as CopyField
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut
import UI.KeyboardShortcut.Key as Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.Sidebar as Sidebar
import UI.ViewMode as ViewMode
import UnisonLocal.Api as LocalApi
import UnisonLocal.Env as Env exposing (Env)
import UnisonLocal.Link as Link
import UnisonLocal.PerspectiveLanding as PerspectiveLanding
import UnisonLocal.Route as Route exposing (Route)
import Url exposing (Url)



-- MODEL


type Modal
    = NoModal
    | FinderModal Finder.Model
    | KeyboardShortcutsModal
    | ReportBugModal
    | PushToShareModal


type alias Model =
    { route : Route
    , codebaseTree : CodebaseTree.Model
    , workspace : Workspace.Model
    , perspectiveLanding : PerspectiveLanding.Model
    , modal : Modal
    , keyboardShortcut : KeyboardShortcut.Model
    , env : Env

    -- This is called "toggled" and not "hidden" because the behavior of
    -- toggling the sidebar on/off is inverse on mobile vs desktop
    , sidebarToggled : Bool
    , isHelpAndResourcesMenuOpen : Bool
    }


init : Env -> Route -> ( Model, Cmd Msg )
init env route =
    let
        codebaseConfig =
            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpoint env

        ( workspace, workspaceCmd ) =
            case route of
                Route.Definition _ ref ->
                    Workspace.init codebaseConfig (Just ref)

                _ ->
                    Workspace.init codebaseConfig Nothing

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init codebaseConfig

        fetchNamespaceDetailsCmd =
            env.perspective
                |> fetchNamespaceDetails
                |> Maybe.map (HttpApi.perform env.api)
                |> Maybe.withDefault Cmd.none

        model =
            { route = route
            , workspace = workspace
            , perspectiveLanding = PerspectiveLanding.init
            , codebaseTree = codebaseTree
            , modal = NoModal
            , keyboardShortcut = KeyboardShortcut.init env.operatingSystem
            , env = env
            , sidebarToggled = False
            , isHelpAndResourcesMenuOpen = False
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
        , Cmd.map WorkspaceMsg workspaceCmd
        , fetchNamespaceDetailsCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ChangePerspective Perspective
    | FetchPerspectiveNamespaceDetailsFinished FQN (Result Http.Error NamespaceDetails)
    | Keydown KeyboardEvent
    | OpenDefinition Reference
    | ShowModal Modal
    | ShowFinder
    | CloseModal
    | ToggleSidebar
    | ToggleHelpAndResourcesMenu
      -- sub msgs
    | FinderMsg Finder.Msg
    | WorkspaceMsg Workspace.Msg
    | PerspectiveLandingMsg PerspectiveLanding.Msg
    | CodebaseTreeMsg CodebaseTree.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ env } as model) =
    let
        codebaseConfig =
            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpoint env
    in
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl env.navKey (Url.toString url) )

                -- External links are handled via target blank and never end up
                -- here
                Browser.External _ ->
                    ( model, Cmd.none )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl env.basePath url

                model2 =
                    { model | route = route }

                newEnv params =
                    if not (Perspective.equals (Perspective.fromParams params) env.perspective) then
                        { env | perspective = Perspective.fromParams params }

                    else
                        env
            in
            case route of
                Route.Definition params ref ->
                    let
                        codebaseConfig_ =
                            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpoint (newEnv params)

                        ( workspace, cmd ) =
                            Workspace.open codebaseConfig_ model.workspace ref

                        model3 =
                            { model2 | workspace = workspace, env = newEnv params }

                        ( model4, fetchPerspectiveCmd ) =
                            fetchPerspectiveAndCodebaseTree env.perspective model3
                    in
                    ( model4, Cmd.batch [ Cmd.map WorkspaceMsg cmd, fetchPerspectiveCmd ] )

                Route.Perspective params ->
                    fetchPerspectiveAndCodebaseTree env.perspective { model2 | env = newEnv params }

        ChangePerspective perspective ->
            navigateToPerspective model perspective

        FetchPerspectiveNamespaceDetailsFinished fqn details ->
            let
                perspective =
                    case env.perspective of
                        Namespace p ->
                            if FQN.equals p.fqn fqn then
                                Namespace { p | details = RemoteData.fromResult details }

                            else
                                env.perspective

                        _ ->
                            env.perspective

                nextEnv =
                    { env | perspective = perspective }
            in
            ( { model | env = nextEnv }, Cmd.none )

        Keydown event ->
            keydown model event

        OpenDefinition ref ->
            navigateToDefinition model ref

        ShowModal modal ->
            ( { model | modal = modal, isHelpAndResourcesMenuOpen = False }, Cmd.none )

        ShowFinder ->
            showFinder model Nothing

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        ToggleSidebar ->
            ( { model | sidebarToggled = not model.sidebarToggled }, Cmd.none )

        ToggleHelpAndResourcesMenu ->
            ( { model | isHelpAndResourcesMenuOpen = not model.isHelpAndResourcesMenuOpen }, Cmd.none )

        -- Sub msgs
        WorkspaceMsg wMsg ->
            let
                ( workspace, wCmd, outMsg ) =
                    Workspace.update codebaseConfig ViewMode.Regular wMsg model.workspace

                model2 =
                    { model | workspace = workspace }

                ( model3, cmd ) =
                    handleWorkspaceOutMsg model2 outMsg
            in
            ( model3, Cmd.batch [ cmd, Cmd.map WorkspaceMsg wCmd ] )

        PerspectiveLandingMsg rMsg ->
            let
                ( perspectiveLanding, outMsg ) =
                    PerspectiveLanding.update rMsg model.perspectiveLanding

                model2 =
                    { model | perspectiveLanding = perspectiveLanding }
            in
            case outMsg of
                PerspectiveLanding.OpenDefinition ref ->
                    navigateToDefinition model2 ref

                PerspectiveLanding.ShowFinderRequest ->
                    showFinder model2 Nothing

                PerspectiveLanding.None ->
                    ( model2, Cmd.none )

        CodebaseTreeMsg cMsg ->
            let
                ( codebaseTree, cCmd, outMsg ) =
                    CodebaseTree.update codebaseConfig cMsg model.codebaseTree

                model2 =
                    { model | codebaseTree = codebaseTree }

                ( model3, cmd ) =
                    case outMsg of
                        CodebaseTree.None ->
                            ( model2, Cmd.none )

                        CodebaseTree.OpenDefinition ref ->
                            -- reset sidebarToggled to close it on mobile, but keep it open on desktop
                            let
                                model4 =
                                    { model2 | sidebarToggled = False }
                            in
                            navigateToDefinition model4 ref

                        CodebaseTree.ChangePerspectiveToNamespace fqn ->
                            fqn
                                |> Perspective.toNamespacePerspective model.env.perspective
                                |> navigateToPerspective model
            in
            ( model3, Cmd.batch [ cmd, Cmd.map CodebaseTreeMsg cCmd ] )

        FinderMsg fMsg ->
            case model.modal of
                FinderModal fModel ->
                    let
                        ( fm, fc, out ) =
                            Finder.update codebaseConfig fMsg fModel
                    in
                    case out of
                        Finder.Remain ->
                            ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fc )

                        Finder.Exit ->
                            ( { model | modal = NoModal }, Cmd.none )

                        Finder.OpenDefinition ref ->
                            navigateToDefinition { model | modal = NoModal } ref

                _ ->
                    ( model, Cmd.none )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )



-- UPDATE HELPERS


navigateToDefinition : Model -> Reference -> ( Model, Cmd Msg )
navigateToDefinition model ref =
    ( model, Route.navigateToDefinition model.env.navKey model.route ref )


navigateToPerspective : Model -> Perspective -> ( Model, Cmd Msg )
navigateToPerspective model perspective =
    let
        -- Update all open references to be hash based to ensure that we can
        -- refresh the page and fetch them appropriately even if they are
        -- outside of the current perspective
        workspace =
            Workspace.replaceWorkspaceItemReferencesWithHashOnly model.workspace

        -- Re-navigate to the currently open definition by hash
        focusedReferenceRoute =
            workspace.workspaceItems
                |> WorkspaceItems.focusedReference
                |> Maybe.map (Route.toDefinition model.route)
                |> Maybe.withDefault model.route

        changeRouteCmd =
            Route.replacePerspective model.env.navKey (Perspective.toParams perspective) focusedReferenceRoute
    in
    ( { model | workspace = workspace }, changeRouteCmd )


fetchPerspectiveAndCodebaseTree : Perspective -> Model -> ( Model, Cmd Msg )
fetchPerspectiveAndCodebaseTree oldPerspective ({ env } as model) =
    let
        codebaseConfig =
            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpoint model.env

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init codebaseConfig

        fetchNamespaceDetailsCmd =
            env.perspective
                |> fetchNamespaceDetails
                |> Maybe.map (HttpApi.perform env.api)
                |> Maybe.withDefault Cmd.none
    in
    if Perspective.needsFetching env.perspective then
        ( { model | codebaseTree = codebaseTree }
        , Cmd.batch
            [ Cmd.map CodebaseTreeMsg codebaseTreeCmd
            , fetchNamespaceDetailsCmd
            ]
        )

    else if not (Perspective.equals oldPerspective env.perspective) then
        ( model, Cmd.map CodebaseTreeMsg codebaseTreeCmd )

    else
        ( model, Cmd.none )


handleWorkspaceOutMsg : Model -> Workspace.OutMsg -> ( Model, Cmd Msg )
handleWorkspaceOutMsg ({ env } as model) out =
    case out of
        Workspace.None ->
            ( model, Cmd.none )

        Workspace.ShowFinderRequest withinNamespace ->
            showFinder model (Just withinNamespace)

        Workspace.Focused ref ->
            ( model, Route.navigateToDefinition env.navKey model.route ref )

        Workspace.Emptied ->
            ( model, Route.navigateToCurrentPerspective env.navKey model.route )

        Workspace.ChangePerspectiveToSubNamespace _ subFqn ->
            let
                perspective =
                    let
                        fullFqn =
                            case env.perspective of
                                Perspective.Namespace { fqn } ->
                                    FQN.append fqn subFqn

                                _ ->
                                    subFqn
                    in
                    Perspective.toNamespacePerspective env.perspective fullFqn
            in
            navigateToPerspective model perspective


keydown : Model -> KeyboardEvent -> ( Model, Cmd Msg )
keydown model keyboardEvent =
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

        KeyboardShortcut.Chord Shift QuestionMark ->
            ( { model | modal = KeyboardShortcutsModal }, Cmd.none )

        KeyboardShortcut.Sequence _ Escape ->
            if model.modal == KeyboardShortcutsModal then
                ( { model | modal = NoModal }, Cmd.none )

            else
                noOp

        _ ->
            if Finder.isShowFinderKeyboardShortcut model.env.operatingSystem shortcut then
                showFinder model Nothing

            else
                noOp


showFinder :
    { m | env : Env, modal : Modal }
    -> Maybe FQN
    -> ( { m | env : Env, modal : Modal }, Cmd Msg )
showFinder model withinNamespace =
    let
        codebaseConfig =
            Env.toCodeConfig LocalApi.codebaseApiEndpointToEndpoint model.env

        options =
            SearchOptions.init model.env.perspective withinNamespace

        ( fm, fcmd ) =
            Finder.init codebaseConfig options
    in
    ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fcmd )



-- EFFECTS


fetchNamespaceDetails : Perspective -> Maybe (HttpApi.ApiRequest NamespaceDetails Msg)
fetchNamespaceDetails perspective =
    case perspective of
        Namespace { fqn } ->
            fqn
                |> LocalApi.namespace perspective
                |> HttpApi.toRequest Namespace.decodeDetails (FetchPerspectiveNamespaceDetailsFinished fqn)
                |> Just

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown
        , Sub.map WorkspaceMsg (Workspace.subscriptions model.workspace)
        ]



-- VIEW


appTitle : Click msg -> AppHeader.AppTitle msg
appTitle click =
    AppHeader.AppTitle click
        (h1 []
            [ text "Unison"
            , span [ class "context unison-local" ] [ text "Local" ]
            ]
        )


appHeader : Bool -> AppHeader.AppHeader Msg
appHeader isHelpAndResourcesMenuOpen =
    let
        actionMenu =
            ActionMenu.items
                { icon = Icon.docs, label = "Docs", click = Link.docs }
                [ { icon = Icon.upload, label = "Unison Share", click = Link.unisonShare }
                , { icon = Icon.bug, label = "Report a Bug", click = Click.onClick (ShowModal ReportBugModal) }
                , { icon = Icon.keyboardKey, label = "Keyboard Shortcuts", click = Click.onClick (ShowModal KeyboardShortcutsModal) }
                , { icon = Icon.unfoldedMap, label = "Code of Conduct", click = Link.codeOfConduct }
                , { icon = Icon.unisonMark, label = "Unison Website", click = Link.website }
                , { icon = Icon.github, label = "Unison on GitHub", click = Link.github }
                ]
                |> ActionMenu.actionMenu ToggleHelpAndResourcesMenu "Help & Resources"
                |> ActionMenu.shouldBeOpen isHelpAndResourcesMenuOpen
                |> ActionMenu.withButtonIcon Icon.questionmark
                |> ActionMenu.view
    in
    { menuToggle = Just ToggleSidebar
    , appTitle = appTitle (Click.Href "/")
    , navigation = Nothing
    , leftSide = []
    , viewMode = ViewMode.Regular
    , rightSide =
        [ actionMenu
        , Button.iconThenLabel (ShowModal PushToShareModal) Icon.upload "Push to Unison Share"
            |> Button.share
            |> Button.small
            |> Button.view
        ]
    }


viewSidebarHeader : Env -> Maybe (Sidebar.SidebarHeader Msg)
viewSidebarHeader env =
    case env.perspective of
        Root _ ->
            Nothing

        Namespace { fqn, details } ->
            let
                -- Imprecise, but close enough, approximation of overflowing,
                -- which results in a slight faded left edge A better way would
                -- be to measure the DOM like we do for overflowing docs, but
                -- thats quite involved...
                isOverflowing =
                    fqn |> FQN.toString |> String.length |> (\l -> l > 20)

                hashvatar =
                    details
                        |> RemoteData.map (Namespace.hash >> Hashvatar.view)
                        |> RemoteData.withDefault Hashvatar.empty

                toClick fqn_ =
                    Click.onClick (ChangePerspective (Perspective.toNamespacePerspective env.perspective fqn_))
            in
            Just
                (Sidebar.header
                    [ div
                        [ classList
                            [ ( "namespace-header", True )
                            , ( "is-overflowing", isOverflowing )
                            ]
                        ]
                        [ hashvatar
                        , h2 [ class "namespace" ] [ FQN.viewClickable toClick fqn ]
                        ]
                    , UI.divider
                    ]
                )


viewMainSidebar : Model -> Sidebar.Sidebar Msg
viewMainSidebar model =
    let
        withHeader header s =
            case header of
                Just h ->
                    Sidebar.withHeader h s

                Nothing ->
                    s
    in
    Sidebar.empty "main-sidebar"
        |> withHeader (viewSidebarHeader model.env)
        |> Sidebar.withSection
            (Sidebar.section
                "Code"
                [ Html.map CodebaseTreeMsg (CodebaseTree.view model.codebaseTree) ]
                |> Sidebar.sectionWithScrollable
                |> Sidebar.sectionWithTitleButton
                    (Button.iconThenLabel ShowFinder Icon.search "Search"
                        |> Button.small
                    )
            )
        |> Sidebar.withToggle { isToggled = model.sidebarToggled, toggleMsg = ToggleSidebar }


viewKeyboardShortcutsModal : OperatingSystem -> KeyboardShortcut.Model -> Html Msg
viewKeyboardShortcutsModal os keyboardShortcut =
    let
        viewRow label instructions =
            div
                [ class "row" ]
                [ label
                , div [ class "instructions" ] instructions
                ]

        viewInstructions label shortcuts =
            viewRow label [ KeyboardShortcut.viewShortcuts keyboardShortcut shortcuts ]

        openFinderInstructions =
            case os of
                MacOS ->
                    [ KeyboardShortcut.Chord Meta (K Key.Lower), KeyboardShortcut.Chord Ctrl (K Key.Lower), KeyboardShortcut.single ForwardSlash ]

                _ ->
                    [ KeyboardShortcut.Chord Ctrl (K Key.Lower), KeyboardShortcut.single ForwardSlash ]

        toggleSidebarInstructions =
            case os of
                MacOS ->
                    [ KeyboardShortcut.Chord Meta (B Key.Lower), KeyboardShortcut.Chord Ctrl (B Key.Lower) ]

                _ ->
                    [ KeyboardShortcut.Chord Ctrl (B Key.Lower) ]

        content =
            Modal.Content
                (section
                    [ class "shortcuts" ]
                    [ div [ class "shortcut-group" ]
                        [ h3 [] [ text "General" ]
                        , viewInstructions (span [] [ text "Keyboard shortcuts", UI.subtle " (this dialog)" ]) [ KeyboardShortcut.single QuestionMark ]
                        , viewInstructions (text "Open Finder") openFinderInstructions
                        , viewInstructions (text "Toggle sidebar") toggleSidebarInstructions
                        , viewInstructions (text "Move focus up") [ KeyboardShortcut.single ArrowUp, KeyboardShortcut.single (K Key.Lower) ]
                        , viewInstructions (text "Move focus down") [ KeyboardShortcut.single ArrowDown, KeyboardShortcut.single (J Key.Lower) ]
                        , viewInstructions (text "Close focused definition") [ KeyboardShortcut.single (X Key.Lower) ]
                        , viewInstructions (text "Expand/Collapse focused definition") [ KeyboardShortcut.single Space ]
                        ]
                    , div [ class "shortcut-group" ]
                        [ h3 [] [ text "Finder" ]
                        , viewInstructions (text "Clear search query") [ KeyboardShortcut.single Escape ]
                        , viewInstructions (span [] [ text "Close", UI.subtle " (when search query is empty)" ]) [ KeyboardShortcut.single Escape ]
                        , viewInstructions (text "Move focus up") [ KeyboardShortcut.single ArrowUp ]
                        , viewInstructions (text "Move focus down") [ KeyboardShortcut.single ArrowDown ]
                        , viewInstructions (text "Open focused definition") [ KeyboardShortcut.single Enter ]
                        , viewRow (text "Open definition")
                            [ KeyboardShortcut.viewBase
                                [ KeyboardShortcut.viewKey os Semicolon False
                                , KeyboardShortcut.viewThen
                                , KeyboardShortcut.viewKeyBase "1-9" False
                                ]
                            ]
                        ]
                    ]
                )
    in
    Modal.modal "help-modal" CloseModal content
        |> Modal.withHeader "Keyboard shortcuts"
        |> Modal.view


viewPushToShareModal : Html Msg
viewPushToShareModal =
    let
        content =
            Modal.Content
                (section
                    []
                    [ p []
                        [ Link.view "Unison Share" Link.share
                        , text " hosts your code in a public namespace under your handle, an example namespace structure of a JSON library might look like this:"
                        ]
                    , div [ class "example-namespace-structure" ]
                        [ UI.inlineCode [] (text "<unison-share-handle>.public.jsonLibrary")
                        ]
                    , p
                        []
                        [ text "Push code to Unison Share with the UCM push command and your Unison Share handle:"
                        ]
                    , CopyField.copyField_
                        Nothing
                        "push.create <handle>.public .localNamespace"
                        |> CopyField.withPrefix ".>"
                        |> CopyField.view
                    , p [ class "help" ]
                        [ Icon.view Icon.bulb
                        , span []
                            [ text "This will push all the contents of "
                            , UI.inlineCode [] (text ".localNamespace")
                            , text " to "
                            , UI.inlineCode [] (text "<handle>.public")
                            , text "."
                            ]
                        ]
                    , div [ class "actions" ] [ Button.button CloseModal "Got It!" |> Button.primary |> Button.medium |> Button.view ]
                    ]
                )
    in
    Modal.modal "push-to-share-modal" CloseModal content
        |> Modal.withHeader "Push your code to Unison Share"
        |> Modal.view


viewReportBugModal : Html Msg
viewReportBugModal =
    let
        content =
            Modal.Content
                (div []
                    [ section []
                        [ p [] [ text "We try our best, but bugs unfortunately creep through :(" ]
                        , p [] [ text "We greatly appreciate feedback and bug reports—its very helpful for providing the best developer experience when working with Unison." ]
                        ]
                    , UI.divider
                    , section [ class "actions" ]
                        [ p [] [ text "Visit our GitHub repositories to report bugs and provide feedback" ]
                        , div [ class "action" ]
                            [ Button.github "unisonweb/unison-local-ui" |> Button.view
                            , text "for reports on"
                            , strong [] [ text "Unison Local" ]
                            , span [ class "subtle" ] [ text "(this UI)" ]
                            ]
                        , div [ class "action" ]
                            [ Button.github "unisonweb/unison" |> Button.view
                            , text "for reports on the"
                            , strong [] [ text "Unison Language" ]
                            , span [ class "subtle" ] [ text "(UCM)" ]
                            ]
                        ]
                    ]
                )
    in
    Modal.modal "report-bug-modal" CloseModal content
        |> Modal.withHeader "Report a Bug"
        |> Modal.view


viewModal :
    { m | env : Env, modal : Modal, keyboardShortcut : KeyboardShortcut.Model }
    -> Html Msg
viewModal model =
    case model.modal of
        NoModal ->
            UI.nothing

        FinderModal m ->
            Html.map FinderMsg (Finder.view m)

        KeyboardShortcutsModal ->
            viewKeyboardShortcutsModal model.env.operatingSystem model.keyboardShortcut

        PushToShareModal ->
            viewPushToShareModal

        ReportBugModal ->
            viewReportBugModal


viewAppLoading : Html msg
viewAppLoading =
    div [ id "app" ]
        [ AppHeader.view (AppHeader.appHeader (appTitle Click.Disabled))
        , PageLayout.view
            (PageLayout.CenteredLayout
                { content = PageContent.empty
                , footer = PageLayout.PageFooter []
                }
            )
        ]


viewAppError : Http.Error -> Html msg
viewAppError error =
    div [ id "app" ]
        [ AppHeader.view (AppHeader.appHeader (appTitle Click.Disabled))
        , PageLayout.view
            (PageLayout.CenteredLayout
                { content =
                    PageContent.oneColumn
                        [ div [ class "app-error" ]
                            [ Icon.view Icon.warn
                            , p [ title (Util.httpErrorToString error) ]
                                [ text "Unison Local could not be started." ]
                            ]
                        ]
                , footer = PageLayout.PageFooter []
                }
            )
        ]


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.route of
                Route.Perspective _ ->
                    PageLayout.SidebarLeftContentLayout
                        { sidebar = viewMainSidebar model
                        , sidebarToggled = model.sidebarToggled
                        , operatingSystem = model.env.operatingSystem
                        , content =
                            PageContent.oneColumn
                                [ Html.map PerspectiveLandingMsg
                                    (PerspectiveLanding.view
                                        model.env.perspective
                                        model.perspectiveLanding
                                    )
                                ]
                        , footer = PageLayout.PageFooter []
                        }

                Route.Definition _ _ ->
                    PageLayout.SidebarEdgeToEdgeLayout
                        { sidebar = viewMainSidebar model
                        , sidebarToggled = model.sidebarToggled
                        , operatingSystem = model.env.operatingSystem
                        , content =
                            PageContent.oneColumn
                                [ Html.map WorkspaceMsg (Workspace.view ViewMode.Regular model.workspace)
                                ]
                        , footer = PageLayout.PageFooter []
                        }
    in
    { title = "Unison Local"
    , body =
        [ div [ id "app" ]
            [ AppHeader.view (appHeader model.isHelpAndResourcesMenuOpen)
            , PageLayout.view page
            , viewModal model
            ]
        ]
    }
