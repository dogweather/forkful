---
title:                "Аналізування html"
html_title:           "Elm: Аналізування html"
simple_title:         "Аналізування html"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Хочете вивчити парсинг HTML у Elm? Ну, очевидно, щоб знаходити інформацію на веб-сторінках і забезпечувати її обробку у вашому додатку.

## Як це зробити

```elm
import Html.Parser as Parser
import Html exposing (text, div, h1)
import Http

url = "https://www.example.com"
response =
  Http.get url text
    |> Task.attempt handleResponse

type Msg
  = ParsingResult (Result Http.Error (List (Html, a)))

parseHtml : String -> List (Html, a)
parseHtml html =
  case Parser.parseHtml html of
    Ok result ->
      result

render : List (Html, a) -> Html
render parsedHtml =
  h1 [] [ text "Парсинг HTML" ]
    :: List.map (\(html, _) -> div [] [ html ]) parsedHtml

subscriptions : model -> Sub Msg
subscriptions _ =
  Sub.none

model : model -> Msg -> (model, Cmd Msg)
model _ _ =
  ( (), Cmd.none )

main =
  Html.beginnerProgram
    { model = model
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

By the way, це єдиний зручний спосіб парсити HTML у Elm, оскільки є платформа для виконання і може бути доступна лише після побудови вже на мобільних пристроях, прикладаються HTML-кодування чи підходиць.



## Deep Dive

У Elm парсер вибирає деяку специфікаційну групу, яка описує те, як цей парсер працює. Її можуть бути і менш особливі, тому що даний інструментарій для создания парсеров за допомогою цих функцій.

Найбільш універсальна картина доступна на веб-сторінці Elm Parsing library, яка дозволяє створювати програми з русскими нумеративными буквами. Але, за допомогою цієї мови, ми можемо створювати не лише такі парсери, чи ситуцій, яким для реальних робіт дуже складно створювати класи або функції на CSS, а й парера, для обробки HTML.

## See Also

- [Elm парсер](http://package.elm-lang.org/packages/elm-lang/elm-parser/latest) - офіційний пакет Elm для створення парсерів.
- [Elm HTML бібліотека](http://package.elm-lang.org/packages/elm-lang/html/latest) - офіційна бібліотека Elm для створення HTML-сторінок.
- [HTML мова для початківців](https://www.w3schools.com/html/) - безкоштовне веб-сайт для вивчення HTML для початківців.