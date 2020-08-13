module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing ( href, class )
import Html.Events exposing (..)
import Http
import RemoteData exposing (RemoteData, WebData)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


---- MODEL ----

type alias News =
    { by : String
     , id : Int
     , score : Int
     , title : String
    }


type alias Model =
    { hackerNews : WebData (List News) }

emptyModel : Model
emptyModel =
    { hackerNews = RemoteData.NotAsked }

init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



---- UPDATE ----


type Msg
    = SendHttpRequest
    | DataReceived (WebData (List News))

dataDecoder: Decoder News
dataDecoder =
    Decode.succeed News
        |> required "by" string
        |> required "id" int
        |> required "score" int
        |> required "title" string

httpCmd : Cmd Msg
httpCmd =
    Http.get
        {url = "https://hacker-news.firebaseio.com/v0/topstories.json"
         , expect =
                list dataDecoder
                    |> Http.expectJson (RemoteData.fromResult >> DataReceived)
                }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ({ model | hackerNews = RemoteData.Loading }, httpCmd )

        DataReceived news ->
            ( { model | hackerNews = news }, Cmd.none )


---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
            [ button [ onClick SendHttpRequest ]
                [ text "Get the news" ]
            , viewNews model
            ]


viewNews : Model -> Html Msg
viewNews model =
    case model.hackerNews of
        RemoteData.NotAsked ->
            text ""
        RemoteData.Loading ->
            text "Loading..."
        RemoteData.Success hackerNews ->
            viewNewsPost hackerNews
        RemoteData.Failure error ->
            viewError (errorMessage error )

viewNewsPost : List News -> Html Msg
viewNewsPost newsList =
    div [ ]
            [ ul []
                ( List.map viewNewsList newsList )
            ]
viewNewsList : News -> Html Msg
viewNewsList model =
    li []
     [ a [ href model.title ]
         [ text model.by ]

         , h6 [] [ text " In ", text (String.fromInt model.score), text  ", Released Date: ", text ( String.fromInt model.id)
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
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
