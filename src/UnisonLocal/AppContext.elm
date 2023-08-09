module UnisonLocal.AppContext exposing (..)

import Browser.Navigation as Nav
import Code.Config
import Code.Perspective exposing (Perspective)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.OperatingSystem as OS exposing (OperatingSystem)
import UnisonLocal.Api as LocalApi
import UnisonLocal.CodeBrowsingContext exposing (CodeBrowsingContext)


type alias AppContext =
    { operatingSystem : OperatingSystem
    , basePath : String
    , api : HttpApi
    , navKey : Nav.Key
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiUrl : String
    }


init : Flags -> Nav.Key -> AppContext
init flags navKey =
    { operatingSystem = OS.fromString flags.operatingSystem
    , basePath = flags.basePath
    , api = HttpApi.httpApi False flags.apiUrl Nothing
    , navKey = navKey
    }


toCodeConfig : AppContext -> CodeBrowsingContext -> Perspective -> Code.Config.Config
toCodeConfig appContext codeBrowsingContext perspective =
    { operatingSystem = appContext.operatingSystem
    , perspective = perspective
    , toApiEndpoint = LocalApi.codebaseApiEndpointToEndpoint codeBrowsingContext
    , api = appContext.api
    }
