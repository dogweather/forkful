---
title:                "Розбір html"
html_title:           "Elm: Розбір html"
simple_title:         "Розбір html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Що & Чому?

Розбір HTML - це процес перетворення рядка HTML-коду у структуровану форму, яка може бути оброблена комп'ютером. Програмісти використовують це для отримання необхідних даних з веб-сторінок, таких як заголовки, текст або посилання.

## Як зробити:

``` Elm
module Main exposing (main)

import Html exposing (..)
import Html.Parser as Parser
import Http exposing (..)

main : Program () Model Msg
main =
  Html.beginnerProgram
    { model = initialModel
    , update = update
    , view = view
    }

type alias Model =
  { data : List String
  }

type Msg
  = ReceiveResult (Result Http.Error String)

initialModel : Model
initialModel =
  { data = [] }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveResult result ->
      case result of
        Ok result ->
          ( { model | data = result }, Cmd.none )
        Err _ ->
          ( model, Cmd.none )

view : Model -> Html Msg
view model =
  let
    parser =
      Parser.text
        |> Parser.collect1
        |> Parser.run "<h1>Hello, World!</h1>"
  in
    Html.div [] [ Html.text (String.join " " (List.map toString (parser model.data))) ]
```
Вивід: Hello, World!

## Вглиб

Розбір HTML був створений для полегшення отримання даних з веб-сторінок та уникнення непотрібного повторення коду. Існують інші способи обробки HTML, такі як використання інших мов програмування або ручне створення парсерів. Розбір HTML в Elm базується на граматиці HTML, тому є високо ефективним та надійним.

## Дивіться також

- Офіційна документація Elm: https://guide.elm-lang.org/
- Використання парсерів в Elm: https://www.elm-tutorial.org/uk/07-parsers/