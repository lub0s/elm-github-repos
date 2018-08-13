module Github exposing (..)

import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode exposing (..)
import String
import Task


-- MODEL


type alias Model =
    { repos : List Repository
    , userName : String
    , error : String
    }


emptyModel : Model
emptyModel =
    { repos = [], userName = "lupajz", error = "" }


type alias Repository =
    { id : Int
    , name : String
    , description : Maybe String
    }


type alias Repositories =
    List Repository


type Action
    = NewInput String
    | NewRepos (Result Http.Error Repositories)
    | Search


urlForName : String -> String
urlForName name =
    "https://api.github.com/users/" ++ name ++ "/repos"


init : ( Model, Cmd Action )
init =
    ( emptyModel, Cmd.none )



-- UPDATE


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        NewInput username ->
            ( Model model.repos username "", fetchReposForUsername username )

        Search ->
            ( model, fetchReposForUsername model.userName )

        NewRepos (Ok repos) ->
            ( Model repos model.userName "", Cmd.none )

        NewRepos (Err error) ->
            ( Model model.repos model.userName ("Something went wrong: " ++ toString error), Cmd.none )


fetchReposForUsername : String -> Cmd Action
fetchReposForUsername username =
    let
        url =
            urlForName (log "username" username)
    in
    Http.send NewRepos (Http.get url decodeRepos)


decodeRepos : Decode.Decoder (List Repository)
decodeRepos =
    Decode.list repoDecoder


repoDecoder : Decode.Decoder Repository
repoDecoder =
    Decode.map3 Repository
        (field "id" int)
        (field "name" string)
        (field "description" (nullable string))



-- VIEW


view : Model -> Html Action
view model =
    div []
        [ section []
            [ Html.Lazy.lazy viewInput model.userName
            , if not (String.isEmpty model.error) then
                text model.error
              else
                text ""
            , ul [] (List.map repoView model.repos)
            ]
        ]


repoView : Repository -> Html Action
repoView repo =
    li [] [ text (repo.name ++ " - " ++ Maybe.withDefault "N/A" repo.description) ]


viewInput : String -> Html Action
viewInput task =
    header
        []
        [ h1 [] [ text "username" ]
        , input
            [ placeholder "username"
            , autofocus True
            , Html.Attributes.value task
            , onInput NewInput
            , onEnter Search
            ]
            []
        ]


onEnter : Action -> Attribute Action
onEnter action =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed action
            else
                Decode.fail "not ENTER"
    in
    on "keydown" (Decode.andThen isEnter keyCode)
