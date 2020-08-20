module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData, WebData)




---- MODEL ----


type alias StoryId =
  Int


type alias Model =
    { stories : WebData (List StoryId)
    }


emptyModel : Model
emptyModel =
    { stories = RemoteData.Loading
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



---- UPDATE ----


type Msg
    = HandleFetchedStoryIds (WebData (List StoryId))


getStoryApi : Int -> Cmd Msg
getStoryApi id =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ ( String.fromInt id ) ++ ".json"

        , expect =
            list int
                |> Http.expectJson (RemoteData.fromResult >> HandleFetchedStoryIds )
        }


itemId : Cmd Msg
itemId =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json"
        , expect =
            list int
                |> Http.expectJson (RemoteData.fromResult >> HandleFetchedStoryIds)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleFetchedStoryIds news ->
            let
                _= Debug.log "hello" news
            in
            ( { model | stories = news }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewNews model
        ]



viewNews : Model -> Html Msg
viewNews model =
    case model.stories of
        RemoteData.NotAsked ->
            div [] [ text "Initializing" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.Success hackerNews ->
            viewNewsPost hackerNews

        RemoteData.Failure error ->
            viewError (errorMessage error)


viewNewsPost : List StoryId -> Html Msg
viewNewsPost newsList =
    div []
        [ ul []
            (List.map viewNewsList newsList)
        ]


viewNewsList : StoryId -> Html Msg
viewNewsList model =
    li []
        [ a []
            [ text (String.fromInt model.id)
            ]
        ]


viewError : String -> Html Msg
viewError error =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ error)
        ]


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( emptyModel, itemId )
        , update = update
        , subscriptions = always Sub.none
        }
