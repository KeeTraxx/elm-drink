module Main exposing (main)

import Browser exposing (Document)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, field, list, map, map5, map6, string, succeed)
import Json.Decode.Extra exposing (andMap)
import Platform.Cmd as Cmd
import Platform.Sub as Sub


type alias Flags =
    ()


type Msg
    = NoOp
    | Failure String
    | FetchDrink
    | GotDrink (Result Http.Error Drinks)


type AppState
    = Loading
    | Showing


type alias Model =
    { activeIndex : Int, drinks : List Drink, error : String, state : AppState }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init initialCount =
    ( { activeIndex = 0, drinks = [], error = "", state = Loading }, fetch )


view : Model -> Document Msg
view model =
    { title = "Drinks desu"
    , body =
        [ div [] [ text model.error ]
        , div [ class "drinks" ] (renderDrink model)
        , button [ onClick FetchDrink ] [ text "Fetch" ]
        ]
    }


stateToString : AppState -> String
stateToString state =
    case state of
        Loading ->
            "loading"

        Showing ->
            "showing"


renderDrink : Model -> List (Html Msg)
renderDrink model =
    List.indexedMap
        (\index drink ->
            div
                [ class
                    (if model.activeIndex == index then
                        "active"

                     else
                        "inactive"
                    )
                ]
                [ h1 [] [ text drink.name ]
                , h2 [] [ text drink.category ]
                , ul [] (List.map (\ing -> li [ class "ingredient" ] [ text ing.name ]) drink.ingredients)
                ]
        )
        model.drinks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Failure error ->
            ( model, Cmd.none )

        FetchDrink ->
            ( { model | state = Loading }, fetch )

        GotDrink result ->
            case result of
                Ok newDrinks ->
                    ( { model | drinks = model.drinks ++ List.map convertDrink newDrinks.drinks, activeIndex = List.length model.drinks, state = Showing }, Cmd.none )

                Err err ->
                    ( { model | error = httpErrorString err, state = Showing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


fetch : Cmd Msg
fetch =
    Http.get
        { url = "https://www.thecocktaildb.com/api/json/v1/1/random.php"
        , expect = Http.expectJson GotDrink drinksDecoder
        }


type alias Drinks =
    { drinks : List RawDrink
    }


type alias RawDrink =
    { name : String
    , category : String
    , thumbnail : String
    , glass : String
    , instructions : String
    , ingredient1 : String
    , ingredient2 : String
    , ingredient3 : String
    , ingredient4 : String
    , ingredient5 : String
    , ingredient6 : String
    , ingredient7 : String
    , ingredient8 : String
    , ingredient9 : String
    , ingredient10 : String
    , ingredient11 : String
    , ingredient12 : String
    , ingredient13 : String
    , ingredient14 : String
    , measure1 : String
    , measure2 : String
    , measure3 : String
    , measure4 : String
    , measure5 : String
    , measure6 : String
    , measure7 : String
    , measure8 : String
    , measure9 : String
    , measure10 : String
    , measure11 : String
    , measure12 : String
    , measure13 : String
    , measure14 : String
    }


type alias Drink =
    { name : String
    , category : String
    , thumbnail : String
    , glass : String
    , instructions : String
    , ingredients : List Ingredient
    }


type alias Ingredient =
    { name : String
    , measure : String
    }


convertDrink : RawDrink -> Drink
convertDrink raw =
    Drink
        raw.name
        raw.category
        raw.thumbnail
        raw.glass
        raw.instructions
        (List.filter nonEmpty
            [ Ingredient raw.ingredient1 raw.measure1
            , Ingredient raw.ingredient2 raw.measure2
            , Ingredient raw.ingredient3 raw.measure3
            , Ingredient raw.ingredient4 raw.measure4
            , Ingredient raw.ingredient5 raw.measure5
            , Ingredient raw.ingredient6 raw.measure6
            , Ingredient raw.ingredient7 raw.measure7
            , Ingredient raw.ingredient8 raw.measure8
            , Ingredient raw.ingredient9 raw.measure9
            , Ingredient raw.ingredient10 raw.measure10
            , Ingredient raw.ingredient11 raw.measure11
            , Ingredient raw.ingredient12 raw.measure12
            , Ingredient raw.ingredient13 raw.measure13
            , Ingredient raw.ingredient14 raw.measure14
            ]
        )


nonEmpty : Ingredient -> Bool
nonEmpty ingredient =
    not (String.isEmpty ingredient.name)


drinksDecoder : Decoder Drinks
drinksDecoder =
    map Drinks
        (field "drinks" (list drinkDecoder))


drinkDecoder : Decoder RawDrink
drinkDecoder =
    succeed RawDrink
        |> andMap (field "strDrink" string)
        |> andMap (field "strCategory" string)
        |> andMap (field "strDrinkThumb" string)
        |> andMap (field "strGlass" string)
        |> andMap (field "strInstructions" string)
        |> andMap (field "strIngredient1" string)
        |> andMap (field "strIngredient2" string)
        |> andMap (field "strIngredient3" string)
        |> andMap (field "strIngredient4" string)
        |> andMap (field "strIngredient5" string)
        |> andMap (field "strIngredient6" string)
        |> andMap (field "strIngredient7" string)
        |> andMap (field "strIngredient8" string)
        |> andMap (field "strIngredient9" string)
        |> andMap (field "strIngredient10" string)
        |> andMap (field "strIngredient11" string)
        |> andMap (field "strIngredient12" string)
        |> andMap (field "strIngredient13" string)
        |> andMap (field "strIngredient14" string)
        |> andMap (field "strMeasure1" string)
        |> andMap (field "strMeasure2" string)
        |> andMap (field "strMeasure3" string)
        |> andMap (field "strMeasure4" string)
        |> andMap (field "strMeasure5" string)
        |> andMap (field "strMeasure6" string)
        |> andMap (field "strMeasure7" string)
        |> andMap (field "strMeasure8" string)
        |> andMap (field "strMeasure9" string)
        |> andMap (field "strMeasure10" string)
        |> andMap (field "strMeasure11" string)
        |> andMap (field "strMeasure12" string)
        |> andMap (field "strMeasure13" string)
        |> andMap (field "strMeasure14" string)


httpErrorString : Error -> String
httpErrorString error =
    case error of
        BadUrl text ->
            "Bad Url: " ++ text

        Timeout ->
            "Http Timeout"

        NetworkError ->
            "Network Error"

        BadStatus response ->
            "Bad Http Status: " ++ toString response

        BadBody message ->
            "Bad Body: "
                ++ message
