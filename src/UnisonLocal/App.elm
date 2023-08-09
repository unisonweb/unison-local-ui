module UnisonLocal.App exposing (..)

import Browser
import Browser.Navigation as Nav
import Code.BranchRef as BranchRef exposing (BranchRef)
import Html exposing (Html, div, h3, section, span, text)
import Html.Attributes exposing (class)
import Lib.OperatingSystem exposing (OperatingSystem(..))
import UI
import UI.KeyboardShortcut as KeyboardShortcut
import UI.KeyboardShortcut.Key as Key exposing (Key(..))
import UI.Modal as Modal
import UnisonLocal.AppDocument as AppDocument
import UnisonLocal.AppHeader as AppHeader
import UnisonLocal.Env exposing (Env)
import UnisonLocal.Page.HomePage as HomePage
import UnisonLocal.Page.NonProjectCodePage as NonProjectCodePage
import UnisonLocal.Page.NotFoundPage as NotFoundPage
import UnisonLocal.Page.ProjectBranchPage as ProjectBranchPage
import UnisonLocal.ProjectName as ProjectName exposing (ProjectName)
import UnisonLocal.Route as Route exposing (Route)
import Url exposing (Url)



-- MODEL


type Page
    = Home HomePage.Model
    | ProjectBranch ProjectName BranchRef Route.CodeRoute ProjectBranchPage.Model
    | NonProjectCode Route.CodeRoute NonProjectCodePage.Model
    | NotFound


type AppModal
    = NoModal
    | KeyboardShortcuts


type alias Model =
    { page : Page
    , env : Env
    , openedAppHeaderMenu : AppHeader.OpenedAppHeaderMenu
    , appModal : AppModal
    }


init : Env -> Route -> ( Model, Cmd Msg )
init env route =
    let
        ( page, cmd ) =
            case route of
                Route.Home ->
                    let
                        ( home, homeCmd ) =
                            HomePage.init env
                    in
                    ( Home home, Cmd.map HomePageMsg homeCmd )

                Route.ProjectBranch projectName branchRef codeRoute ->
                    let
                        ( project, projectBranchCmd ) =
                            ProjectBranchPage.init env projectName branchRef codeRoute
                    in
                    ( ProjectBranch projectName branchRef codeRoute project, Cmd.map ProjectBranchPageMsg projectBranchCmd )

                Route.NonProjectCode codeRoute ->
                    let
                        ( nonProjectCode, nonProjectCodeCmd ) =
                            NonProjectCodePage.init env codeRoute
                    in
                    ( NonProjectCode codeRoute nonProjectCode, Cmd.map NonProjectCodePageMsg nonProjectCodeCmd )

                Route.NotFound _ ->
                    ( NotFound, Cmd.none )

        model =
            { page = page
            , env = env
            , openedAppHeaderMenu = AppHeader.NoneOpened
            , appModal = NoModal
            }
    in
    ( model, cmd )



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | AcceptWelcomeTerms
    | ToggleHelpAndResourcesMenu
    | ShowKeyboardShortcuts
    | CloseModal
      -- Sub msgs
    | HomePageMsg HomePage.Msg
    | ProjectBranchPageMsg ProjectBranchPage.Msg
    | NonProjectCodePageMsg NonProjectCodePage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ env } as model) =
    case ( model.page, msg ) of
        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl env.navKey (Url.toString url) )

                -- External links are handled via target blank and never end up
                -- here
                Browser.External _ ->
                    ( model, Cmd.none )

        ( _, UrlChanged url ) ->
            let
                route =
                    Route.fromUrl env.basePath url

                ( m, c ) =
                    case route of
                        Route.Home ->
                            let
                                ( home, cmd ) =
                                    HomePage.init model.env
                            in
                            ( { model | page = Home home }, Cmd.map HomePageMsg cmd )

                        Route.ProjectBranch projectName branchRef codeRoute ->
                            case model.page of
                                ProjectBranch currentProjectName currentBranchRef _ projectModel ->
                                    if ProjectName.equals currentProjectName projectName && BranchRef.equals currentBranchRef branchRef then
                                        let
                                            ( project, cmd ) =
                                                ProjectBranchPage.updateSubPage model.env projectName branchRef projectModel codeRoute
                                        in
                                        ( { model | page = ProjectBranch projectName branchRef codeRoute project }, Cmd.map ProjectBranchPageMsg cmd )

                                    else
                                        let
                                            ( project, cmd ) =
                                                ProjectBranchPage.init model.env projectName branchRef codeRoute
                                        in
                                        ( { model | page = ProjectBranch projectName branchRef codeRoute project }, Cmd.map ProjectBranchPageMsg cmd )

                                _ ->
                                    let
                                        ( project, cmd ) =
                                            ProjectBranchPage.init model.env projectName branchRef codeRoute
                                    in
                                    ( { model | page = ProjectBranch projectName branchRef codeRoute project }, Cmd.map ProjectBranchPageMsg cmd )

                        Route.NonProjectCode codeRoute ->
                            case model.page of
                                NonProjectCode _ npcModel ->
                                    let
                                        ( npc, cmd ) =
                                            NonProjectCodePage.updateSubPage model.env npcModel codeRoute
                                    in
                                    ( { model | page = NonProjectCode codeRoute npc }, Cmd.map NonProjectCodePageMsg cmd )

                                _ ->
                                    let
                                        ( npc, cmd ) =
                                            NonProjectCodePage.init model.env codeRoute
                                    in
                                    ( { model | page = NonProjectCode codeRoute npc }, Cmd.map NonProjectCodePageMsg cmd )

                        Route.NotFound _ ->
                            ( { model | page = NotFound }, Cmd.none )
            in
            ( m, c )

        ( _, ToggleHelpAndResourcesMenu ) ->
            let
                openedAppHeaderMenu =
                    if model.openedAppHeaderMenu == AppHeader.HelpAndResourcesMenu then
                        AppHeader.NoneOpened

                    else
                        AppHeader.HelpAndResourcesMenu
            in
            ( { model | openedAppHeaderMenu = openedAppHeaderMenu }, Cmd.none )

        ( _, ShowKeyboardShortcuts ) ->
            ( { model | openedAppHeaderMenu = AppHeader.NoneOpened, appModal = KeyboardShortcuts }, Cmd.none )

        ( _, CloseModal ) ->
            ( { model | appModal = NoModal }, Cmd.none )

        -- Sub msgs
        ( Home m, HomePageMsg cMsg ) ->
            let
                ( home, cmd ) =
                    HomePage.update env cMsg m
            in
            ( { model | page = Home home }, Cmd.map HomePageMsg cmd )

        ( ProjectBranch projectName branchRef codeRoute m, ProjectBranchPageMsg pMsg ) ->
            let
                ( projectBranch, cmd ) =
                    ProjectBranchPage.update env projectName branchRef codeRoute pMsg m
            in
            ( { model | page = ProjectBranch projectName branchRef codeRoute projectBranch }, Cmd.map ProjectBranchPageMsg cmd )

        ( NonProjectCode codeRoute m, NonProjectCodePageMsg npcMsg ) ->
            let
                ( nonProjectCode, cmd ) =
                    NonProjectCodePage.update env codeRoute npcMsg m
            in
            ( { model | page = NonProjectCode codeRoute nonProjectCode }, Cmd.map NonProjectCodePageMsg cmd )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        ProjectBranch _ _ _ pb ->
            Sub.map ProjectBranchPageMsg (ProjectBranchPage.subscriptions pb)

        NonProjectCode _ npc ->
            Sub.map NonProjectCodePageMsg (NonProjectCodePage.subscriptions npc)

        _ ->
            Sub.none



-- VIEW


viewKeyboardShortcutsModal : OperatingSystem -> Html Msg
viewKeyboardShortcutsModal os =
    let
        -- The shortcut views requires a model, but we don't really need it for this kind of overview
        keyboardShortcut =
            KeyboardShortcut.init os

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
                        [ h3 [] [ text "Browsing Code" ]
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


view : Model -> Browser.Document Msg
view model =
    let
        env =
            model.env

        appHeaderContext =
            { openedAppHeaderMenu = model.openedAppHeaderMenu
            , toggleHelpAndResourcesMenuMsg = ToggleHelpAndResourcesMenu
            , showKeyboardShortcutsModalMsg = ShowKeyboardShortcuts
            }

        appDocument =
            case model.page of
                Home home ->
                    AppDocument.map HomePageMsg (HomePage.view home)

                ProjectBranch projectName branchRef _ projectModel ->
                    AppDocument.map ProjectBranchPageMsg (ProjectBranchPage.view env projectName branchRef projectModel)

                NonProjectCode _ nonProjectCodeModel ->
                    AppDocument.map NonProjectCodePageMsg (NonProjectCodePage.view env nonProjectCodeModel)

                NotFound ->
                    NotFoundPage.view

        -- Overwrite Modal with any app level, user initiated modal
        appDocumentWithModal =
            case model.appModal of
                NoModal ->
                    appDocument

                KeyboardShortcuts ->
                    { appDocument | modal = Just (viewKeyboardShortcutsModal env.operatingSystem) }
    in
    AppDocument.view appHeaderContext appDocumentWithModal
