module UnisonLocal.ProjectName exposing (..)

import Code.Project.ProjectSlug as ProjectSlug exposing (ProjectSlug)
import Json.Decode as Decode
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util as Util


type ProjectName
    = ProjectName (Maybe UserHandle) ProjectSlug



-- CREATE


{-| ProjectNames can include a handle when being parsed.

valid examples are:

  - "myproject", "myBranch", "mybr4nch"
  - "my-project", "my\_branch", "-my---branch", "\_my-branch"
    (for some reason elm-format wont allow having underscores without a backslash infront)
  - "@owner/my-project"

invalid examples

  - "my/project"
  - "my@project"
  - "@owner/my/project"
  - "@owner@my/project"
  - "@owner@my/project"
  - "my project"
  - "mybr%@#nch"

-}
fromString : String -> Maybe ProjectName
fromString raw =
    let
        parts =
            String.split "/" raw
    in
    case parts of
        [ h, s ] ->
            Maybe.map2
                (\h_ s_ -> ProjectName (Just h_) s_)
                (UserHandle.fromString h)
                (ProjectSlug.fromString s)

        [ s ] ->
            Maybe.map (ProjectName Nothing) (ProjectSlug.fromString s)

        _ ->
            Nothing



-- HELPERS


equals : ProjectName -> ProjectName -> Bool
equals (ProjectName handleA slugA) (ProjectName handleB slugB) =
    let
        handleEquals =
            Maybe.map2 UserHandle.equals handleA handleB
                |> Maybe.withDefault False
    in
    handleEquals && ProjectSlug.equals slugA slugB


toString : ProjectName -> String
toString (ProjectName handle slug) =
    let
        handle_ =
            case handle of
                Just h ->
                    UserHandle.toString h ++ "/"

                Nothing ->
                    ""
    in
    handle_ ++ ProjectSlug.toString slug


toApiString : ProjectName -> String
toApiString (ProjectName handle slug) =
    let
        handle_ =
            case handle of
                Just h ->
                    UserHandle.toString h ++ "%2F"

                Nothing ->
                    ""
    in
    handle_ ++ ProjectSlug.toString slug



-- DECODE


decode : Decode.Decoder ProjectName
decode =
    Decode.map fromString Decode.string
        |> Decode.andThen (Util.decodeFailInvalid "Invalid ProjectName")
