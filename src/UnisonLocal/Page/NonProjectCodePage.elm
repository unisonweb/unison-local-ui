module UnisonLocal.Page.NonProjectCodePage exposing (..)

import Html exposing (text)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageFooter(..))
import UnisonLocal.AppDocument as AppDocument exposing (AppDocument)
import UnisonLocal.AppHeader as AppHeader
import UnisonLocal.CodeBrowsingContext as CodeBrowsingContext
import UnisonLocal.Env exposing (Env)
import UnisonLocal.Page.CodePage as CodePage
import UnisonLocal.Route as Route



-- MODEL


type alias Model =
    { code : CodePage.Model }


init : Env -> Route.CodeRoute -> ( Model, Cmd Msg )
init env codeRoute =
    let
        context =
            CodeBrowsingContext.nonProjectCode

        ( code, cmd ) =
            CodePage.init env context codeRoute
    in
    ( { code = code }, Cmd.map CodePageMsg cmd )



-- UPDATE


type Msg
    = NoOp
    | CodePageMsg CodePage.Msg


update : Env -> Route.CodeRoute -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ model =
    ( model, Cmd.none )


{-| Pass through to CodePage. Used by App when routes change
-}
updateSubPage : Env -> Model -> Route.CodeRoute -> ( Model, Cmd Msg )
updateSubPage env model codeRoute =
    let
        codeBrowsingContext =
            CodeBrowsingContext.nonProjectCode

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


view : Model -> AppDocument Msg
view _ =
    let
        appHeader =
            AppHeader.appHeader

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn [ text "non project code" ])
                (PageFooter [])
                |> PageLayout.withSubduedBackground
    in
    AppDocument.appDocument
        "non-project-code-page"
        "TODO Non-Project Code"
        appHeader
        (PageLayout.view page)
