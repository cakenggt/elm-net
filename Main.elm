-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Net
import Json.Decode exposing (decodeString, list, float)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { net : Net.Net,
    result : List Float,
    input : List (List Float),
    target : List (List Float),
    error : Float,
    backpropIter : Int
  }


init : (Model, Cmd Msg)
init =
  (Model (Net.createNetDeterministic 2 2 1) [0] [[0, 0], [1, 0], [0, 1], [1, 1]] [[0], [1], [1], [0]] 0 10000, Cmd.none)



-- UPDATE


type Msg
  = Randomize
  | NewNet Net.Net
  | Forward
  | NewInput String
  | NewTarget String
  | Backprop
  | NewBackprop String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Randomize ->
      (model, Net.createNetRandom 2 2 2 NewNet)

    NewNet newNet ->
      ({model | net = newNet}, Cmd.none)

    NewInput str ->
        ({model | input = stringToList str}, Cmd.none)

    NewTarget str ->
        ({model | target = stringToList str}, Cmd.none)

    Forward ->
        let
            inputHead = List.head model.input
            input = case inputHead of
                Just n ->
                    n
                Nothing ->
                    [0,0]
            targetHead = List.head model.target
            target = case targetHead of
                Just n ->
                    n
                Nothing ->
                    [0]
        in
            ({model | result = Net.forwardPass model.net input, error = Net.getTotalError model.net input target}, Cmd.none)

    Backprop ->
        let
            inputHead = List.head model.input
            input = case inputHead of
                Just n ->
                    n
                Nothing ->
                    [0,0]
            targetHead = List.head model.target
            target = case targetHead of
                Just n ->
                    n
                Nothing ->
                    [0]
        in
            ({model | net = Net.backpropagateSet model.net 1 model.input model.target model.backpropIter, error = Net.getTotalError model.net input target}, Cmd.none)

    NewBackprop str ->
        let
            iterCase = String.toInt str
            iter = case iterCase of
                Ok n ->
                    n
                Err _ ->
                    0
        in
            ({model | backpropIter = iter}, Cmd.none)

stringToList : String -> List (List Float)
stringToList str =
    case decodeString (Json.Decode.list (Json.Decode.list float)) str of
        Ok ls ->
            ls
        Err _ ->
            [[]]
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString model.net) ]
    , h2 [] [ text (toString model.result)]
    , button [ onClick Randomize ] [ text "Randomize" ]
    , input [ onInput NewInput, value (toString model.input) ] []
    , input [ onInput NewTarget, value (toString model.target) ] []
    , button [ onClick Forward ] [ text "Forward" ]
    , button [ onClick Backprop ] [ text "Backprop" ]
    , input [ onInput NewBackprop, value (toString model.backpropIter) ] []
    , h2 [] [ text (toString model.error) ]
    ]
