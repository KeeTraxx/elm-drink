module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, field, list, map, null, oneOf, string, succeed)
import Json.Decode.Extra exposing (andMap)
import Platform.Cmd as Cmd
import Platform.Sub as Sub


type alias Flags =
    ()


type Msg
    = NoOp
    | Failure String
    | FetchDrink
    | Previous
    | Next
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
        , div [ class "drinks" ]
            (case model.state of
                Loading ->
                    [ div [] [ text "Loading..." ] ]

                Showing ->
                    renderDrink model
            )
        , div [ class "controls" ]
            [ div
                [ onClick Previous
                , style "visibility"
                    (if model.activeIndex == 0 then
                        "hidden"

                     else
                        ""
                    )
                ]
                [ text "❮" ]
            , div [ onClick Next ] [ text "❯" ]
            ]
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
                , img [ src drink.thumbnail, width 150 ] []
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

        Previous ->
            ( if model.activeIndex > 0 then
                { model | activeIndex = model.activeIndex - 1 }

              else
                model
            , Cmd.none
            )

        Next ->
            if model.activeIndex + 1 == List.length model.drinks then
                ( { model | state = Loading }, fetch )

            else
                ( { model | activeIndex = model.activeIndex + 1 }, Cmd.none )

        GotDrink result ->
            case result of
                Ok newDrinks ->
                    ( { model | drinks = model.drinks ++ List.map convertDrink newDrinks.drinks, activeIndex = List.length model.drinks, state = Showing }, Cmd.none )

                Err err ->
                    ( { model | error = httpErrorString err, state = Showing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Loading ->
            Sub.none

        Showing ->
            Browser.Events.onKeyDown keyPressDecoder


keyPressDecoder : Decoder Msg
keyPressDecoder =
    map keyPressToMsg (field "key" string)


keyPressToMsg : String -> Msg
keyPressToMsg key =
    case key of
        "ArrowLeft" ->
            Previous

        "ArrowRight" ->
            Next

        _ ->
            NoOp


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
        |> andMap (field "strIngredient1" nullOrStringToEmptyString)
        |> andMap (field "strIngredient2" nullOrStringToEmptyString)
        |> andMap (field "strIngredient3" nullOrStringToEmptyString)
        |> andMap (field "strIngredient4" nullOrStringToEmptyString)
        |> andMap (field "strIngredient5" nullOrStringToEmptyString)
        |> andMap (field "strIngredient6" nullOrStringToEmptyString)
        |> andMap (field "strIngredient7" nullOrStringToEmptyString)
        |> andMap (field "strIngredient8" nullOrStringToEmptyString)
        |> andMap (field "strIngredient9" nullOrStringToEmptyString)
        |> andMap (field "strIngredient10" nullOrStringToEmptyString)
        |> andMap (field "strIngredient11" nullOrStringToEmptyString)
        |> andMap (field "strIngredient12" nullOrStringToEmptyString)
        |> andMap (field "strIngredient13" nullOrStringToEmptyString)
        |> andMap (field "strIngredient14" nullOrStringToEmptyString)
        |> andMap (field "strMeasure1" nullOrStringToEmptyString)
        |> andMap (field "strMeasure2" nullOrStringToEmptyString)
        |> andMap (field "strMeasure3" nullOrStringToEmptyString)
        |> andMap (field "strMeasure4" nullOrStringToEmptyString)
        |> andMap (field "strMeasure5" nullOrStringToEmptyString)
        |> andMap (field "strMeasure6" nullOrStringToEmptyString)
        |> andMap (field "strMeasure7" nullOrStringToEmptyString)
        |> andMap (field "strMeasure8" nullOrStringToEmptyString)
        |> andMap (field "strMeasure9" nullOrStringToEmptyString)
        |> andMap (field "strMeasure10" nullOrStringToEmptyString)
        |> andMap (field "strMeasure11" nullOrStringToEmptyString)
        |> andMap (field "strMeasure12" nullOrStringToEmptyString)
        |> andMap (field "strMeasure13" nullOrStringToEmptyString)
        |> andMap (field "strMeasure14" nullOrStringToEmptyString)


nullOrStringToEmptyString : Decoder String
nullOrStringToEmptyString =
    oneOf [ null "", string ]


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
