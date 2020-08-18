module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import RemoteData exposing (RemoteData, WebData)



---- MODEL ----


type alias News =
    { id : Int }


type alias Model =
    { hackerNews : WebData (List News) }


emptyModel : Model
emptyModel =
    { hackerNews = RemoteData.Loading }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



---- UPDATE ----


type Msg
    = DataReceived (WebData (List News))


dataDecoder : Decoder News
dataDecoder =
    int |> andThen (\id_ -> succeed (News id_))


httpCmd : Cmd Msg
httpCmd =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty"
        , expect =
            list dataDecoder
                |> Http.expectJson (RemoteData.fromResult >> DataReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataReceived news ->
            ( { model | hackerNews = news }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewNews model
        ]


viewNews : Model -> Html Msg
viewNews model =
    case model.hackerNews of
        RemoteData.NotAsked ->
            div [] [ text "Initializing" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.Success hackerNews ->
            viewNewsPost hackerNews

        RemoteData.Failure error ->
            viewError (errorMessage error)


viewNewsPost : List News -> Html Msg
viewNewsPost newsList =
    div []
        [ ul []
            (List.map viewNewsList newsList)
        ]


viewNewsList : News -> Html Msg
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
