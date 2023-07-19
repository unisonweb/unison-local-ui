module UnisonLocal.AppHeader exposing (..)

import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class)
import UI.ActionMenu as ActionMenu
import UI.AppHeader exposing (AppHeader, AppTitle(..))
import UI.Click as Click exposing (Click)
import UI.Icon as Icon
import UnisonLocal.Link as Link


type AppHeader msg
    = Disabled
    | AppHeader
        { leftSide : List (Html msg)
        }



-- CREATE


empty : AppHeader msg
empty =
    Disabled


appHeader : AppHeader msg
appHeader =
    empty


map : (msgA -> msgB) -> AppHeader msgA -> AppHeader msgB
map toMsgB appHeader_ =
    case appHeader_ of
        Disabled ->
            Disabled

        AppHeader ah ->
            AppHeader
                { leftSide = List.map (Html.map toMsgB) ah.leftSide
                }



-- MODIFY


withLeftSide : List (Html msg) -> AppHeader msg -> AppHeader msg
withLeftSide leftSide appHeader_ =
    case appHeader_ of
        Disabled ->
            AppHeader { leftSide = leftSide }

        AppHeader ah ->
            AppHeader { ah | leftSide = leftSide }



-- VIEW


appTitle : Click msg -> AppTitle msg
appTitle click =
    AppTitle click
        (h1 []
            [ text "Unison"
            , span [ class "context unison-local" ] [ text "Local" ]
            ]
        )


{-| Represents app level context, that is injected at render time
-}
type OpenedAppHeaderMenu
    = NoneOpened
    | HelpAndResourcesMenu


type alias AppHeaderContext msg =
    { openedAppHeaderMenu : OpenedAppHeaderMenu
    , toggleHelpAndResourcesMenuMsg : msg
    , showKeyboardShortcutsModalMsg : msg
    }


isHelpAndResourcesMenuOpen : OpenedAppHeaderMenu -> Bool
isHelpAndResourcesMenuOpen openedAppHeaderMenu =
    openedAppHeaderMenu == HelpAndResourcesMenu


view : AppHeaderContext msg -> AppHeader msg -> Html msg
view ctx appHeader_ =
    case appHeader_ of
        Disabled ->
            viewBlank

        AppHeader { leftSide } ->
            let
                helpAndResources =
                    ActionMenu.items
                        (ActionMenu.optionItem Icon.docs "Docs" Link.docs)
                        [ ActionMenu.optionItem Icon.bug "Report a Bug" Link.reportBug
                        , ActionMenu.optionItem Icon.keyboardKey "Keyboard Shortcuts" (Click.onClick ctx.showKeyboardShortcutsModalMsg)
                        , ActionMenu.optionItem Icon.unfoldedMap "Code of Conduct" Link.codeOfConduct
                        , ActionMenu.optionItem Icon.unisonMark "Unison Website" Link.website
                        , ActionMenu.optionItem Icon.github "Unison on GitHub" Link.github
                        ]
                        |> ActionMenu.fromButton ctx.toggleHelpAndResourcesMenuMsg "Help & Resources"
                        |> ActionMenu.shouldBeOpen (isHelpAndResourcesMenuOpen ctx.openedAppHeaderMenu)
                        |> ActionMenu.withButtonIcon Icon.questionmark
                        |> ActionMenu.withMaxWidth (ActionMenu.Rem 12)
                        |> ActionMenu.view
                        |> (\hr -> div [ class "help-and-resources" ] [ hr ])
            in
            UI.AppHeader.appHeader (appTitle (Click.href "/"))
                |> UI.AppHeader.withLeftSide leftSide
                |> UI.AppHeader.withRightSide [ helpAndResources ]
                |> UI.AppHeader.view


viewBlank : Html msg
viewBlank =
    appTitle Click.Disabled
        |> UI.AppHeader.appHeader
        |> UI.AppHeader.view
