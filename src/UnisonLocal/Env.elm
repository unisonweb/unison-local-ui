module UnisonLocal.Env exposing (..)

import Browser.Navigation as Nav
import Code.CodebaseApi as CodebaseApi
import Code.Config
import Code.Perspective exposing (Perspective)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.OperatingSystem as OS exposing (OperatingSystem)


type alias Env =
    { operatingSystem : OperatingSystem
    , basePath : String
    , api : HttpApi
    , navKey : Nav.Key
    , perspective : Perspective
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiUrl : String
    }


init : Flags -> Nav.Key -> Perspective -> Env
init flags navKey perspective =
    { operatingSystem = OS.fromString flags.operatingSystem
    , basePath = flags.basePath
    , api = HttpApi.httpApi False flags.apiUrl Nothing
    , navKey = navKey
    , perspective = perspective
    }


toCodeConfig : CodebaseApi.ToApiEndpoint -> Env -> Code.Config.Config
toCodeConfig toApiEndpoint env =
    { operatingSystem = env.operatingSystem
    , perspective = env.perspective
    , toApiEndpoint = toApiEndpoint
    , api = env.api
    }
