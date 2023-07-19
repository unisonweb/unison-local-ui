module UnisonLocal.AppDocument exposing
    ( AppDocument
    , appDocument
    , map
    , view
    , withModal
    , withModal_
    )

import Browser exposing (Document)
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import UI
import UnisonLocal.AppHeader as AppHeader exposing (AppHeader, AppHeaderContext)



{-

   AppDocument
   ===========

   Very similar to Browser.Document, but includes a common app title and app
   frame, as well as slots for header, page, and modals.

   TODO: Figure out an extensible version that can be ported to ui-core
-}


type alias AppDocument msg =
    { pageId : String
    , title : String
    , appHeader : AppHeader msg
    , page : Html msg
    , modal : Maybe (Html msg)
    }



-- CREATE


appDocument : String -> String -> AppHeader msg -> Html msg -> AppDocument msg
appDocument pageId title appHeader page =
    { pageId = pageId
    , title = title
    , appHeader = appHeader
    , page = page
    , modal = Nothing
    }



-- MODIFY


withModal : Html msg -> AppDocument msg -> AppDocument msg
withModal modal appDoc =
    withModal_ (Just modal) appDoc


withModal_ : Maybe (Html msg) -> AppDocument msg -> AppDocument msg
withModal_ modal appDoc =
    { appDoc | modal = modal }



-- MAP


map : (msgA -> msgB) -> AppDocument msgA -> AppDocument msgB
map toMsgB { pageId, title, appHeader, page, modal } =
    { pageId = pageId
    , title = title
    , appHeader = AppHeader.map toMsgB appHeader
    , page = Html.map toMsgB page
    , modal = Maybe.map (Html.map toMsgB) modal
    }



-- VIEW


view : AppHeaderContext msg -> AppDocument msg -> Document msg
view appHeaderCtx { pageId, title, appHeader, page, modal } =
    { title = title ++ " | Unison Local"
    , body =
        [ div
            [ id "app"
            , class pageId
            ]
            [ AppHeader.view appHeaderCtx appHeader
            , page
            , Maybe.withDefault UI.nothing modal
            ]
        ]
    }
