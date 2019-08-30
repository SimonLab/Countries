module Main exposing (main)

import Browser
import Countries exposing (Country, countries)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Flags =
    ()



-- Model


type alias Model =
    { countries : List Country, filter : String }


initModel : Model
initModel =
    { countries = countries, filter = "" }



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



-- Update


type Msg
    = UpdateFilter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFilter f ->
            ( { model | filter = f }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "List of countries"
    , body =
        [ h1 [] [ text "Countries" ]
        , input [ onInput UpdateFilter, placeholder "Filter list of countries" ] []
        , showCountries model
        ]
    }


showCountries : Model -> Html Msg
showCountries model =
    ul [] <| List.map showCountry <| List.filter (filterCountry model.filter) model.countries


filterCountry : String -> Country -> Bool
filterCountry filter country =
    if String.isEmpty filter then
        True

    else
        String.startsWith (String.toLower filter) (String.toLower country.name)
            || String.startsWith (String.toLower filter) (String.toLower country.code)


showCountry : Country -> Html Msg
showCountry country =
    li []
        [ img [ src <| "./assets/" ++ country.code ++ ".png" ] []
        , text <| country.name ++ " ( " ++ country.code ++ " )"
        ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
