module UnisonLocal.Route exposing
    ( CodeRoute(..)
    , Route(..)
    , codeRoot
    , definition
    , fromUrl
    , home
    , navigate
    , nonProjectCode
    , nonProjectCodeRoot
    , nonProjectDefinition
    , projectBranch
    , projectBranchDefinition
    , projectBranchRoot
    , replacePerspective
    , toRoute
    , toUrlString
    )

import Browser.Navigation as Nav
import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Definition.Reference exposing (Reference(..))
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.HashQualified exposing (HashQualified(..))
import Code.Perspective as Perspective exposing (Perspective, PerspectiveParams(..))
import Code.Project.ProjectSlug as ProjectSlug
import Code.UrlParsers as UP exposing (b, branchRef, projectSlug, reference, s, slash, userHandle)
import Lib.UserHandle as UserHandle
import List.Nonempty as NEL
import Parser exposing ((|.), (|=), Parser, end, oneOf, succeed)
import UnisonLocal.Project exposing (ProjectName(..))
import Url exposing (Url)
import Url.Builder exposing (relative)



{-

   Routing
   =======

   URL Scheme
   ----------

   Directly on the codebase
   /[latest|:codebase-hash]/[namespaces|types|terms]/[:namespace-name|:definition-name|:definition-hash]


   Within a namespace
   /[latest|:codebase-hash]/[namespaces]/[:namespace-name]/-/[types|terms]/[:definition-name|:definition-hash]


   Relative examples
   -----------------

   Top level of a Codebase:                     /
   Top level of a Codebase:                     /latest
   With namespace context:                      /latest/namespaces/base/List
   Definitions:                                 /latest/[types|terms]/base/List/map
   Disambiguated definitions:                   /latest/[types|terms]/base/List@je2wR6
   Definitions within namespace:                /latest/namespaces/base/List/-/[types|terms]/map
   Disambiguated definitions within namespace:  /latest/namespaces/base/List/-/[types|terms]/map@je2wR6


   Absolute examples
   -----------------

   Definitions:                                 /@785shikvuihsdfd/[types|terms]/@jf615sgdkvuihskrt
   Disambiguated definitions:                   /@785shikvuihsdfd/[types|terms]/Nonempty/map@dkqA42
   With namespace context:                      /@785shikvuihsdfd/namespaces/base/List
   Definitions within namespace:                /@785shikvuihsdfd/namespaces/base/List/-/[types|terms]/base/List/map
   Disambiguated definitions within namespace:  /@785shikvuihsdfd/namespaces/base/List/-/[types|terms]/Nonempty/map@dkqA42


   Note: @785shikvuihsdfd here refers to the hash of the codebase

-}


type CodeRoute
    = CodeRoot PerspectiveParams
    | Definition PerspectiveParams Reference


type Route
    = Home
    | ProjectBranch ProjectName BranchRef CodeRoute
    | NonProjectCode CodeRoute
    | NotFound String



-- CREATE ---------------------------------------------------------------------


home : Route
home =
    Home


nonProjectCode : CodeRoute -> Route
nonProjectCode codeRoute =
    NonProjectCode codeRoute


nonProjectDefinition : Perspective -> Reference -> Route
nonProjectDefinition pers ref =
    let
        pp =
            Perspective.toParams pers
    in
    NonProjectCode (Definition pp ref)


nonProjectCodeRoot : Perspective -> Route
nonProjectCodeRoot pers =
    let
        pp =
            Perspective.toParams pers
    in
    NonProjectCode (CodeRoot pp)


projectBranch : ProjectName -> BranchRef -> CodeRoute -> Route
projectBranch projectName branchRef_ codeRoute =
    ProjectBranch projectName branchRef_ codeRoute


projectBranchDefinition : ProjectName -> BranchRef -> Perspective -> Reference -> Route
projectBranchDefinition projectName branchRef_ pers ref =
    let
        pp =
            Perspective.toParams pers
    in
    ProjectBranch projectName branchRef_ (Definition pp ref)


projectBranchRoot : ProjectName -> BranchRef -> Perspective -> Route
projectBranchRoot projectName branchRef pers =
    let
        pp =
            Perspective.toParams pers
    in
    ProjectBranch projectName branchRef (CodeRoot pp)


definition : Perspective -> Reference -> CodeRoute
definition pers ref =
    Definition (Perspective.toParams pers) ref


codeRoot : Perspective -> CodeRoute
codeRoot pers =
    CodeRoot (Perspective.toParams pers)


replacePerspective : Maybe Reference -> Perspective -> CodeRoute
replacePerspective ref pers =
    let
        pp =
            Perspective.toParams pers
    in
    case ref of
        Just r ->
            Definition pp r

        Nothing ->
            CodeRoot pp



-- PARSER ---------------------------------------------------------------------


codeRootParser : Parser CodeRoute
codeRootParser =
    succeed CodeRoot |. slash |= UP.perspectiveParams |. end


definitionParser : Parser CodeRoute
definitionParser =
    succeed Definition |. slash |= UP.perspectiveParams |. slash |= reference |. end


code : Parser CodeRoute
code =
    oneOf [ b codeRootParser, b definitionParser ]


homeParser : Parser Route
homeParser =
    succeed Home |. slash |. end


projectNameParser : Parser ProjectName
projectNameParser =
    oneOf
        [ b (succeed (Just >> ProjectName) |= userHandle |. slash |= projectSlug)
        , b (succeed (ProjectName Nothing) |= projectSlug)
        ]


projectBranchParser : Parser Route
projectBranchParser =
    succeed ProjectBranch |. slash |. s "projects" |. slash |= projectNameParser |. slash |= branchRef |= code


nonProjectParser : Parser Route
nonProjectParser =
    succeed NonProjectCode |. slash |. s "non-project-code" |= code


toRoute : Parser Route
toRoute =
    oneOf [ b homeParser, b projectBranchParser, b nonProjectParser ]


{-| In environments like Unison Local, the UI is served with a base path

This means that a route to a definition might look like:

  - "/:some-token/ui/latest/terms/base/List/map"
    (where "/:some-token/ui/" is the base path.)

The base path is determined outside of the Elm app using the <base> tag in the
<head> section of the document. The Browser uses this tag to prefix all links.

The base path must end in a slash for links to work correctly, but our parser
expects a path to starts with a slash. When parsing the URL we thus pre-process
the path to strip the base path and ensure a slash prefix before we parse.

-}
fromUrl : String -> Url -> Route
fromUrl basePath url =
    let
        stripBasePath path =
            if basePath == "/" then
                path

            else
                String.replace basePath "" path

        ensureSlashPrefix path =
            if String.startsWith "/" path then
                path

            else
                "/" ++ path

        parse url_ =
            Result.withDefault
                (NotFound (Url.toString url))
                (Parser.run toRoute url_)
    in
    url
        |> .path
        |> stripBasePath
        |> ensureSlashPrefix
        |> parse



-- TRANSFORM


toUrlString : Route -> String
toUrlString route =
    let
        hqToPath hq =
            case hq of
                NameOnly fqn ->
                    fqn |> FQN.toUrlSegments |> NEL.toList

                HashOnly h ->
                    [ Hash.toUrlString h ]

                HashQualified _ h ->
                    -- Currently not supported, since we favor the hash
                    [ Hash.toUrlString h ]

        namespaceSuffix =
            ";"

        -- used to mark the end of a namespace FQN
        perspectiveParamsToPath pp includeNamespacesSuffix =
            case pp of
                ByRoot Perspective.Relative ->
                    [ "latest" ]

                ByRoot (Perspective.Absolute hash) ->
                    [ Hash.toUrlString hash ]

                ByNamespace Perspective.Relative fqn ->
                    if includeNamespacesSuffix then
                        "latest" :: "namespaces" :: NEL.toList (FQN.segments fqn) ++ [ namespaceSuffix ]

                    else
                        "latest" :: "namespaces" :: NEL.toList (FQN.segments fqn)

                -- Currently the model supports Absolute URLs (aka Permalinks),
                -- but we don't use it since Unison Share does not support any
                -- history, meaning that everytime we deploy Unison Share, the
                -- previous versions of the codebase are lost.
                -- It's fully intended for this feature to be brought back
                ByNamespace (Perspective.Absolute hash) fqn ->
                    if includeNamespacesSuffix then
                        Hash.toUrlString hash :: "namespaces" :: NEL.toList (FQN.segments fqn) ++ [ namespaceSuffix ]

                    else
                        Hash.toUrlString hash :: "namespaces" :: NEL.toList (FQN.segments fqn)

        codeRouteToPath cr =
            case cr of
                CodeRoot pp ->
                    perspectiveParamsToPath pp False

                Definition pp ref ->
                    case ref of
                        TypeReference hq ->
                            perspectiveParamsToPath pp True ++ ("types" :: hqToPath hq)

                        TermReference hq ->
                            perspectiveParamsToPath pp True ++ ("terms" :: hqToPath hq)

                        AbilityConstructorReference hq ->
                            perspectiveParamsToPath pp True ++ ("ability-constructors" :: hqToPath hq)

                        DataConstructorReference hq ->
                            perspectiveParamsToPath pp True ++ ("data-constructors" :: hqToPath hq)

        projectNameToPath (ProjectName handle slug) =
            case handle of
                Just h ->
                    [ UserHandle.toString h, ProjectSlug.toString slug ]

                Nothing ->
                    [ ProjectSlug.toString slug ]

        path =
            case route of
                Home ->
                    []

                ProjectBranch name branchRef cr ->
                    "projects" :: projectNameToPath name ++ BranchRef.toUrlPath branchRef ++ codeRouteToPath cr

                NonProjectCode cr ->
                    "non-project-code" :: codeRouteToPath cr

                NotFound _ ->
                    []
    in
    relative path []



-- EFFECTS


navigate : Nav.Key -> Route -> Cmd msg
navigate navKey route =
    route
        |> toUrlString
        |> Nav.pushUrl navKey
