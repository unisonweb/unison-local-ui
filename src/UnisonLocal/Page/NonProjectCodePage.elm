module UnisonLocal.Page.NonProjectCodePage exposing (..)

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
    = CodePageMsg CodePage.Msg


update : Env -> Route.CodeRoute -> Msg -> Model -> ( Model, Cmd Msg )
update env codeRoute msg model =
    case msg of
        CodePageMsg codePageMsg ->
            let
                ( codePage_, codePageCmd ) =
                    CodePage.update env CodeBrowsingContext.nonProjectCode codeRoute codePageMsg model.code
            in
            ( { model | code = codePage_ }
            , Cmd.map CodePageMsg codePageCmd
            )


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
subscriptions model =
    Sub.map CodePageMsg (CodePage.subscriptions model.code)



-- VIEW


view : Env -> Model -> AppDocument Msg
view env model =
    let
        appHeader =
            AppHeader.appHeader

        ( codePage_, modal_ ) =
            CodePage.view env
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
