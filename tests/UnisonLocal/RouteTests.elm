module UnisonLocal.RouteTests exposing (..)

import Expect
import Test exposing (..)
import UnisonLocal.Route as Route
import Url exposing (Url)


homeRoute : Test
homeRoute =
    describe "Route.fromUrl : home route"
        [ test "Matches root to Home" <|
            \_ ->
                let
                    url =
                        mkUrl "/"
                in
                Expect.equal Route.Home (Route.fromUrl "" url)
        ]


mkUrl : String -> Url
mkUrl path =
    { protocol = Url.Https
    , host = "unison-lang.org"
    , port_ = Just 443
    , path = path
    , query = Nothing
    , fragment = Nothing
    }
