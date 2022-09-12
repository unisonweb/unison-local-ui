module UnisonLocal.Api exposing
    ( codebaseApiEndpointToEndpoint
    , codebaseHash
    , namespace
    )

import Code.CodebaseApi as CodebaseApi
import Code.Definition.Reference as Reference
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.Namespace.NamespaceRef as NamespaceRef
import Code.Perspective as Perspective exposing (Perspective(..))
import Code.Syntax as Syntax
import Lib.HttpApi exposing (Endpoint(..))
import Maybe.Extra as MaybeE
import Regex
import Url.Builder exposing (QueryParameter, int, string)


codebaseHash : Endpoint
codebaseHash =
    GET { path = [ "list" ], queryParams = [] }


namespace : Perspective -> FQN -> Endpoint
namespace perspective fqn =
    let
        queryParams =
            [ toRootBranch (Perspective.rootPerspective perspective) ]
    in
    GET
        { path = [ "namespaces", FQN.toString fqn ]
        , queryParams = MaybeE.values queryParams
        }


codebaseApiEndpointToEndpoint : CodebaseApi.CodebaseEndpoint -> Endpoint
codebaseApiEndpointToEndpoint cbEndpoint =
    case cbEndpoint of
        CodebaseApi.Find { perspective, withinFqn, limit, sourceWidth, query } ->
            let
                params =
                    case withinFqn of
                        Just fqn ->
                            [ toRootBranch (Perspective.rootPerspective perspective)
                            , Just (relativeTo fqn)
                            ]
                                |> MaybeE.values

                        Nothing ->
                            perspectiveToQueryParams perspective

                width =
                    case sourceWidth of
                        Syntax.Width w ->
                            w
            in
            GET
                { path = [ "find" ]
                , queryParams =
                    [ int "limit" limit
                    , int "renderWidth" width
                    , string "query" query
                    ]
                        ++ params
                }

        CodebaseApi.Browse { perspective, ref } ->
            let
                namespace_ =
                    ref
                        |> Maybe.map NamespaceRef.toString
                        |> Maybe.map (string "namespace")
                        |> Maybe.map (\qp -> [ qp ])
            in
            GET
                { path = [ "list" ]
                , queryParams = Maybe.withDefault [] namespace_ ++ perspectiveToQueryParams perspective
                }

        CodebaseApi.Definition { perspective, ref } ->
            let
                re =
                    Maybe.withDefault Regex.never (Regex.fromString "#[d|a|](\\d+)$")

                stripConstructorPositionFromHash =
                    Regex.replace re (always "")
            in
            [ Reference.toApiUrlString ref ]
                |> List.map stripConstructorPositionFromHash
                |> List.map (string "names")
                |> (\names -> GET { path = [ "getDefinition" ], queryParams = names ++ perspectiveToQueryParams perspective })



-- QUERY PARAMS ---------------------------------------------------------------


perspectiveToQueryParams : Perspective -> List QueryParameter
perspectiveToQueryParams perspective =
    case perspective of
        Root p ->
            MaybeE.values [ toRootBranch p ]

        Namespace d ->
            [ toRootBranch d.root, Just (relativeTo d.fqn) ] |> MaybeE.values


toRootBranch : Perspective.RootPerspective -> Maybe QueryParameter
toRootBranch rootPerspective =
    case rootPerspective of
        Perspective.Relative ->
            Nothing

        Perspective.Absolute h ->
            Just (rootBranch h)


rootBranch : Hash -> QueryParameter
rootBranch hash =
    string "rootBranch" (hash |> Hash.toString)


relativeTo : FQN -> QueryParameter
relativeTo fqn =
    string "relativeTo" (fqn |> FQN.toString)
