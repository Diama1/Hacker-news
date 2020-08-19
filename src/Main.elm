module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, int, list, string, succeed, field)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData, WebData)



---- MODEL ----


type alias Ids =
    { id : Int }

type alias Story =
    { by : String
    , descendants : Int
    , id : Int
    , kids : List Int
    , score : Int
    , time : Int
    , title : String
    , url : String
    }

type alias Model =
    { hackerNews : WebData (List Story)
     , storyIds : WebData ( List Ids )
     }


emptyModel : Model
emptyModel =
    { hackerNews = RemoteData.Loading,
     storyIds = RemoteData.Loading }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



---- UPDATE ----


type Msg
    = StoryIds (WebData (List Ids))
    | TopStory ( WebData ( List Story ) )


idDecoder : Decoder Ids
idDecoder =
    int |> andThen (\id_ -> succeed (Ids id_))

dataDecoder : Decoder Story
dataDecoder =
    Decode.map8 Story
        (field "by" Decode.string)
        (field "descendants" Decode.int)
        (field "id" Decode.int)
        (field "kids" (Decode.list Decode.int))
        (field "score" Decode.int)
        (field "time" Decode.int)
        (field "title" Decode.string)
        (field "url" Decode.string)


httpCmd : Int -> Cmd Msg
httpCmd id =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ ( String.fromInt id ) ++ ".json"

        , expect =
            list dataDecoder
                |> Http.expectJson (RemoteData.fromResult >> TopStory)
        }

itemId : Cmd Msg
itemId =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty"
        , expect =
            list idDecoder
                |> Http.expectJson (RemoteData.fromResult >> StoryIds)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoryIds id ->
            ( {model | storyIds = id}, Cmd.none )

        TopStory news ->
            ( { model | hackerNews = news }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewNews model
        ]


viewNews : Model -> Html Msg
viewNews model =
    case model.storyIds of
        RemoteData.NotAsked ->
            div [] [ text "Initializing" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.Success hackerNews ->
            viewNewsPost hackerNews

        RemoteData.Failure error ->
            viewError (errorMessage error)


viewNewsPost : List Ids -> Html Msg
viewNewsPost newsList =
    div []
        [ ul []
            (List.map viewNewsList newsList)
        ]


viewNewsList : Ids -> Html Msg
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
        , init = \_ -> ( emptyModel, httpCmd )
        , update = update
        , subscriptions = always Sub.none
        }
