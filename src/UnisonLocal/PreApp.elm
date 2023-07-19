{- Right now this is just a passthrough, but it should really be used to fetch
   the current session like on Share
-}


module UnisonLocal.PreApp exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, id, title)
import Http
import Lib.Util as Util
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonLocal.App as App
import UnisonLocal.AppHeader as AppHeader
import UnisonLocal.Env as Env exposing (Flags)
import UnisonLocal.Route as Route exposing (Route)
import Url exposing (Url)


type Model
    = Initializing PreEnv
    | InitializationError PreEnv Http.Error
    | Initialized App.Model


type alias PreEnv =
    { flags : Flags
    , route : Route
    , navKey : Nav.Key
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        route =
            Route.fromUrl flags.basePath url

        env =
            Env.init flags navKey

        ( app, cmd ) =
            App.init env route
    in
    ( Initialized app, Cmd.map AppMsg cmd )


type Msg
    = AppMsg App.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, AppMsg appMsg ) ->
            case model of
                Initialized a ->
                    let
                        ( app, cmd ) =
                            App.update appMsg a
                    in
                    ( Initialized app, Cmd.map AppMsg cmd )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized app ->
            Sub.map AppMsg (App.subscriptions app)

        _ ->
            Sub.none


viewAppLoading : Html msg
viewAppLoading =
    div [ id "app" ]
        [ AppHeader.viewBlank
        , PageLayout.view
            (PageLayout.centeredLayout
                PageContent.empty
                (PageLayout.PageFooter [])
            )
        ]


viewAppError : Http.Error -> Html msg
viewAppError error =
    div [ id "app" ]
        [ AppHeader.viewBlank
        , PageLayout.view
            (PageLayout.centeredLayout
                (PageContent.oneColumn
                    [ div [ class "app-error" ]
                        [ Icon.view Icon.warn
                        , p [ title (Util.httpErrorToString error) ]
                            [ text "Unison Local could not be started." ]
                        ]
                    ]
                )
                (PageLayout.PageFooter [])
            )
        ]


view : Model -> Browser.Document Msg
view model =
    case model of
        Initializing _ ->
            { title = "Loading.. | Unison Local"
            , body = [ viewAppLoading ]
            }

        InitializationError _ error ->
            { title = "Application Error | Unison Local"
            , body = [ viewAppError error ]
            }

        Initialized appModel ->
            let
                app =
                    App.view appModel
            in
            { title = app.title
            , body = List.map (Html.map AppMsg) app.body
            }
