module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (toInt)
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

type alias Model =
  { task : List Todo
  , inputs : Todo
  }

init : Model
init =
  Model [] (Todo "" "" 0)


type Msg
  = AddTodo
  | DeleteTodo Int
  | ChangeTitle String
  | ChangeDescription String
  | ChangeEstimate String


update : Msg -> Model -> Model
update msg model =
  case msg of
    AddTodo ->
      let
        oldInputs = model.inputs
        task = Todo oldInputs.title oldInputs.description oldInputs.estimate
        newInputs =
          { oldInputs | title = "", description = "", estimate = 0 }
      in
        { model | inputs = newInputs, task = task :: model.task }

    DeleteTodo idx ->
      { model | task = (removeAt idx model.task) }

    ChangeTitle newTitle ->
      let
        oldInputs = model.inputs
        newInputs =
          {  oldInputs | title = newTitle }
      in
        { model | inputs = newInputs }

    ChangeDescription newDescription ->
      let
        oldInputs = model.inputs
        newInputs =
          {  oldInputs | description = newDescription }
      in
        { model | inputs = newInputs }

    ChangeEstimate newEstimate ->
      let
        toInt = case String.toInt newEstimate of
                  Just n -> n
                  Nothing -> 0
        oldInputs = model.inputs
        newInputs =
          {  oldInputs | estimate = toInt }
      in
        { model | inputs = newInputs }


-- modelの一覧をmapで表示すること
-- 別のフォームを作成すること

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ h1 [] [ text "タスクの追加" ]
        , p [] [ text "タスク名" ]
        , input [ type_ "text"
                , placeholder "ご飯を作る"
                , value model.inputs.title
                , onInput ChangeTitle
                ] []
        , p [] [ text "タスクの詳細" ]
        , textarea [ placeholder "美味しいオムライスを作って食べること"
                   , value model.inputs.description
                   , onInput ChangeDescription
                   ] []
        , p [] [ text "Estimate" ]
        , input [ type_ "number"
                , placeholder "Estimate"
                , value (String.fromInt model.inputs.estimate)
                , Html.Attributes.min "0"
                , onInput ChangeEstimate
                ] []
        , button [ onClick AddTodo ] [ text "追加" ]
      ]
      , h2 [] [ text "タスクの一覧" ]
      , div [] (List.indexedMap (\i todo -> todoList todo i) model.task)
    ]


todoList : Todo -> Int -> Html Msg
todoList todo idx =
  div []
  [ h3 [] [ text ("タスク名: " ++ todo.title) ]
  , p [] [ text ("タスクの詳細: " ++ todo.description) ]
  , p [] [ text ("Estimate: " ++ String.fromInt todo.estimate) ]
  , button [ onClick (DeleteTodo idx) ] [ text "削除" ]
  ]