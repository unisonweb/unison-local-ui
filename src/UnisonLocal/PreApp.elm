module UnisonLocal.PreApp exposing (..)

import Browser
import Browser.Navigation as Nav
import Code.Perspective as Perspective exposing (PerspectiveParams)
import Html
import Http
import UnisonLocal.App as App
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
    , perspectiveParams : PerspectiveParams
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        route =
            Route.fromUrl flags.basePath url

        preEnv =
            { flags = flags
            , route = route
            , navKey = navKey
            , perspectiveParams = Route.perspectiveParams route
            }

        perspectiveToAppInit perspective =
            let
                env =
                    Env.init preEnv.flags preEnv.navKey perspective

                ( app, cmd ) =
                    App.init env preEnv.route
            in
            ( Initialized app, Cmd.map AppMsg cmd )
    in
    preEnv.perspectiveParams
        |> Perspective.fromParams
        |> perspectiveToAppInit


type Msg
    = AppMsg App.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppMsg appMsg ->
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


view : Model -> Browser.Document Msg
view model =
    case model of
        Initializing _ ->
            { title = "Loading.."
            , body = [ App.viewAppLoading ]
            }

        InitializationError _ error ->
            { title = "Application Error"
            , body = [ App.viewAppError error ]
            }

        Initialized appModel ->
            let
                app =
                    App.view appModel
            in
            { title = app.title
            , body = List.map (Html.map AppMsg) app.body
            }
