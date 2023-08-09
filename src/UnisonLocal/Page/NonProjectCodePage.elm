module UnisonLocal.Page.NonProjectCodePage exposing (..)

import UI.PageLayout as PageLayout exposing (PageFooter(..))
import UnisonLocal.AppContext exposing (AppContext)
import UnisonLocal.AppDocument as AppDocument exposing (AppDocument)
import UnisonLocal.AppHeader as AppHeader
import UnisonLocal.CodeBrowsingContext as CodeBrowsingContext
import UnisonLocal.Page.CodePage as CodePage
import UnisonLocal.Route as Route



-- MODEL


type alias Model =
    { code : CodePage.Model }


init : AppContext -> Route.CodeRoute -> ( Model, Cmd Msg )
init appContext codeRoute =
    let
        context =
            CodeBrowsingContext.nonProjectCode

        ( code, cmd ) =
            CodePage.init appContext context codeRoute
    in
    ( { code = code }, Cmd.map CodePageMsg cmd )



-- UPDATE


type Msg
    = CodePageMsg CodePage.Msg


update : AppContext -> Route.CodeRoute -> Msg -> Model -> ( Model, Cmd Msg )
update appContext codeRoute msg model =
    case msg of
        CodePageMsg codePageMsg ->
            let
                ( codePage_, codePageCmd ) =
                    CodePage.update appContext CodeBrowsingContext.nonProjectCode codeRoute codePageMsg model.code
            in
            ( { model | code = codePage_ }
            , Cmd.map CodePageMsg codePageCmd
            )


{-| Pass through to CodePage. Used by App when routes change
-}
updateSubPage : AppContext -> Model -> Route.CodeRoute -> ( Model, Cmd Msg )
updateSubPage appContext model codeRoute =
    let
        codeBrowsingContext =
            CodeBrowsingContext.nonProjectCode

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


view : AppContext -> Model -> AppDocument Msg
view appContext model =
    let
        appHeader =
            AppHeader.appHeader

        ( codePage_, modal_ ) =
            CodePage.view appContext
                CodePageMsg
                CodeBrowsingContext.nonProjectCode
                model.code
    in
    AppDocument.appDocument
        "non-project-code-page"
        "Non Project Code"
        appHeader
        (PageLayout.view codePage_)
        |> AppDocument.withModal_ modal_
