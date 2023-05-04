module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import List.Extra exposing (removeAt)

main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

type alias Todo =
  { title : String
  , description : String
  , estimate : Int
  }

updateTitle : String -> Todo -> Todo
updateTitle newTitle todo =
  { todo | title = newTitle }

updateDescription : String -> Todo -> Todo
updateDescription newDescription todo =
  { todo | description = newDescription }

updateEstimate : Int -> Todo -> Todo
updateEstimate newEstimate todo =
  { todo | estimate = newEstimate }


newTodoValidate : Todo -> Bool
newTodoValidate todo =
  String.isEmpty todo.title || String.isEmpty todo.description


type alias Model =
  { task : List Todo
  , newTodo : Todo
  , isValid : Bool
  }

emptyInput : Todo
emptyInput =
  { title = ""
  , description = ""
  , estimate = 0
  }

init : Model
init =
  { task = []
  , newTodo = emptyInput
  , isValid = True
  }


type Msg
  = AddTodo
  | DeleteTodo Int
  | UpdateTitle String
  | UpdateDescription String
  | UpdateEstimate String


update : Msg -> Model -> Model
update msg model =
  let
    newTodo =
      model.newTodo
  in
  case msg of
      AddTodo ->
        if newTodoValidate model.newTodo then
          { model | isValid = False }
        else
          { model | newTodo = emptyInput
                , task = newTodo :: model.task
                , isValid = True
                }

      DeleteTodo idx ->
        { model | task = removeAt idx model.task }

      UpdateTitle newTitle ->
        { model | newTodo = updateTitle newTitle newTodo, isValid = True }

      UpdateDescription newDescription ->
        { model | newTodo = updateDescription newDescription newTodo, isValid = True }

      UpdateEstimate newEstimateText ->
        let
          newEstimate =
            Maybe.withDefault 0 <| String.toInt newEstimateText
        in
        { model | newTodo = updateEstimate newEstimate newTodo }


view : Model -> Html Msg
view model =
  div []
    [ div []
      [ h1 [] [ text "タスクの追加" ]
        , p [] [ text "タスク名" ]
        , input [ type_ "text"
                , placeholder "ご飯を作る"
                , value model.newTodo.title
                , onInput UpdateTitle
                ] []
        , stringValidation model.isValid model.newTodo.title "タスク名を入力してください"
        , p [] [ text "タスクの詳細" ]
        , textarea [ placeholder "美味しいオムライスを作って食べること"
                   , value model.newTodo.description
                   , onInput UpdateDescription
                   ] []
        , stringValidation model.isValid model.newTodo.description "タスクの詳細を入力してください"
        , p [] [ text "Estimate" ]
        , input [ type_ "number"
                , placeholder "Estimate"
                , value (String.fromInt model.newTodo.estimate)
                , Html.Attributes.min "0"
                , onInput UpdateEstimate
                ] []
        , button [ onClick AddTodo ] [ text "追加" ]
      ]
      , h2 [] [ text "タスクの一覧" ]
      , div [] <| List.indexedMap (\i todo -> todoList todo i) model.task
    ]


todoList : Todo -> Int -> Html Msg
todoList todo idx =
  div []
    [ h3 [] [ text ("タスク名: " ++ todo.title) ]
    , p [] [ text ("タスクの詳細: " ++ todo.description) ]
    , p [] [ text ("Estimate: " ++ String.fromInt todo.estimate) ]
    , button [ onClick (DeleteTodo idx) ] [ text "削除" ]
    ]


stringValidation : Bool -> String -> String -> Html Msg
stringValidation isValid value errorText =
  if not isValid && String.isEmpty value then
    div [ style "color" "red" ] [ text errorText ]
  else
    div [ style "color" "black" ] []
