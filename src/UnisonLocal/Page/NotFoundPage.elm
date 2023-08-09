module UnisonLocal.Page.NotFoundPage exposing (..)

import Html exposing (text)
import UI.Card as Card
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UnisonLocal.AppDocument exposing (AppDocument)
import UnisonLocal.AppHeader as AppHeader


view : AppDocument msg
view =
    let
        content =
            [ Card.card [ text "Sorry, we can't find that page." ] |> Card.view
            ]

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn content
                    |> PageContent.withPageTitle
                        (PageTitle.title "Page not found" |> PageTitle.withIcon Icon.warn)
                )
                (PageLayout.PageFooter [])
    in
    { pageId = "not-found"
    , title = "Page not found"
    , appHeader = AppHeader.empty
    , page = PageLayout.view page
    , modal = Nothing
    }
