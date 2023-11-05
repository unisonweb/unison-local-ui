module UnisonLocal.Link exposing (..)

import Code.BranchRef exposing (BranchRef)
import Code.Perspective as Perspective
import Html exposing (Html, text)
import UI.Click as Click exposing (Click)
import UnisonLocal.ProjectName exposing (ProjectName)
import UnisonLocal.Route as Route exposing (Route)



{-

   Link
   ====

   Various UI.Click link helpers for Routes and external links

-}
-- ROUTES


projectBranchRoot : ProjectName -> BranchRef -> Click msg
projectBranchRoot projectName branchRef =
    let
        pers =
            Perspective.relativeRootPerspective
    in
    Route.projectBranchRoot projectName branchRef pers
        |> toClick


toClick : Route -> Click msg
toClick =
    Route.toUrlString >> Click.href



-- EXTERNAL


share : Click msg
share =
    Click.externalHref "https://share.unison-lang.org"


website : Click msg
website =
    Click.externalHref "https://unison-lang.org"


github : Click msg
github =
    Click.externalHref "https://github.com/unisonweb/unison"


reportBug : Click msg
reportBug =
    Click.externalHref "https://github.com/unisonweb/unison/issues/new"


docs : Click msg
docs =
    Click.externalHref "https://unison-lang.org/docs"


tour : Click msg
tour =
    Click.externalHref "https://unison-lang.org/docs/tour"


codeOfConduct : Click msg
codeOfConduct =
    Click.externalHref "https://www.unison-lang.org/community/code-of-conduct/"


status : Click msg
status =
    Click.externalHref "https://unison.statuspage.io"


slack : Click msg
slack =
    Click.externalHref "https://unison-lang.com/slack"


termsOfService : Click msg
termsOfService =
    Click.externalHref "https://share.unison-lang.org/terms-of-service"


privacyPolicy : Click msg
privacyPolicy =
    Click.externalHref "https://share.unison-lang.org/privacy-policy"


roadmap : Click msg
roadmap =
    Click.externalHref "https://share.unison-lang.org/roadmap"


unisonShare : Click msg
unisonShare =
    Click.externalHref "https://share.unison-lang.org"



-- VIEW


view : String -> Click msg -> Html msg
view label click =
    Click.view [] [ text label ] click
