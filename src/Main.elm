module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import RemoteData exposing (RemoteData, WebData)


---- MODEL ----

type alias AllNews =
    { id : Int
    , by : String
    , score : Maybe Int
    , title : String
    }
type News
    = Loading
    | GetNews ( WebData (List AllNews))


type alias Model =
    { hackerNews : News }

emptyModel : Model
emptyModel =
    { hackerNews = Loading }

init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hacker News" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
