---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:52.259232-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Elm \u0442\u0440\u0435\u0431\u0443\u0435\u0442, \u0447\u0442\u043E\
  \u0431\u044B \u043F\u043E\u0431\u043E\u0447\u043D\u044B\u0435 \u044D\u0444\u0444\
  \u0435\u043A\u0442\u044B, \u0442\u0430\u043A\u0438\u0435 \u043A\u0430\u043A HTTP-\u0437\
  \u0430\u043F\u0440\u043E\u0441\u044B, \u0441\u0442\u0440\u0443\u043A\u0442\u0443\
  \u0440\u0438\u0440\u043E\u0432\u0430\u043B\u0438\u0441\u044C \u043A\u0430\u043A\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u044B. \u0412\u044B \u0431\u0443\u0434\u0435\
  \u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u043C\u043E\u0434\u0443\u043B\u044C `Http` \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:44.899002-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0442\u0440\u0435\u0431\u0443\u0435\u0442, \u0447\u0442\u043E\u0431\
  \u044B \u043F\u043E\u0431\u043E\u0447\u043D\u044B\u0435 \u044D\u0444\u0444\u0435\
  \u043A\u0442\u044B, \u0442\u0430\u043A\u0438\u0435 \u043A\u0430\u043A HTTP-\u0437\
  \u0430\u043F\u0440\u043E\u0441\u044B, \u0441\u0442\u0440\u0443\u043A\u0442\u0443\
  \u0440\u0438\u0440\u043E\u0432\u0430\u043B\u0438\u0441\u044C \u043A\u0430\u043A\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u044B."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Elm требует, чтобы побочные эффекты, такие как HTTP-запросы, структурировались как команды. Вы будете использовать модуль `Http` для получения данных и обработки ответа.

```Elm

module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

type Msg
    = GotText (Result Http.Error String)

init : ( Model, Cmd Msg )
init =
    ( Model ""
    , fetchPage "https://api.example.com/data"
    )

fetchPage : String -> Cmd Msg
fetchPage url =
    Http.get { url = url, expect = Http.expectString GotText }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok data) ->
            ( { model | content = data }, Cmd.none )

        GotText (Err _) ->
            ( { model | content = "Ошибка: Не удалось загрузить страницу." }, Cmd.none )

view : Model -> Html Msg
view model =
    text model.content

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }

```

При успешной загрузке `content` в вашей модели будет содержать содержимое страницы. В случае ошибки он будет содержать простое сообщение об ошибке.

## Подробнее
Elm рассматривает побочные эффекты как Данные, что означает, что HTTP-запросы управляются средой выполнения Elm, а не напрямую в вашем коде. Исторически это отличалось от языков, таких как JavaScript, где побочные эффекты более свободны. Альтернативы в других языках могли бы быть `fetch` в JavaScript или `requests` в Python. Подход Elm обеспечивает предсказуемость и удобство обслуживания вашего приложения, кодируя побочные эффекты в типы и используя централизованную функцию `update` для управления изменениями.

Модуль `Http` не всегда существовал в Elm. Ранние версии создавали свой собственный AJAX, что было неудобно. Теперь `Http` предоставляет набор функций для обработки различных случаев, таких как ожидание JSON или строк, что делает его более удобным для пользователя.

С точки зрения реализации, когда вы вызываете `fetchPage`, Elm отправляет сообщение в вашу функцию `update` с результатом. Это будет либо `Ok data`, если операция выполнена успешно, либо `Err error`, если произошла ошибка. Вы используете сопоставление по образцу для этих исходов и соответственно обновляете вашу `Model` и представление.

## См. также
- Документация по HTTP-пакету Elm: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Руководство Elm по эффектам: [https://guide.elm-lang.org/effects/](https://guide.elm-lang.org/effects/)
- Декодирование JSON в Elm (для случаев, когда получаемые данные не являются простой строкой): [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
