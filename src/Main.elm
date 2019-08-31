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
    { countries : List Country, filter : Maybe String }


initModel : Model
initModel =
    { countries = countries, filter = Nothing }



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
        UpdateFilter filter ->
            let
                f =
                    if String.isEmpty filter then
                        Nothing

                    else
                        Just filter
            in
            ( { model | filter = f }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Browser.Document Msg
view model =
    let
        title =
            "European Union Countries"

        titleWithFitler =
            case model.filter of
                Nothing ->
                    title

                Just f ->
                    title ++ " | " ++ f
    in
    { title = title
    , body =
        [ h1 [] [ text titleWithFitler ]
        , input [ onInput UpdateFilter, placeholder "Filter the list" ] []
        , showCountries model
        ]
    }


showCountries : Model -> Html Msg
showCountries model =
    ul [] <|
        List.map showCountry <|
            List.filter (filterCountry model.filter) model.countries



-- List.map showCountry <| List.filter (filterCountry model.filter) model.countries


filterCountry : Maybe String -> Country -> Bool
filterCountry filter country =
    case filter of
        Nothing ->
            True

        Just f ->
            String.startsWith (String.toLower f) (String.toLower country.name)
                || String.startsWith (String.toLower f) (String.toLower country.code)


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
