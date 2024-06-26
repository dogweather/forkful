---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:51:03.218398-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0438\u0441\u0442\u043E \u0444\u0443\u043D\u043A\u0446\u0438\
  \u043E\u043D\u0430\u043B\u044C\u043D\u0430\u044F \u043F\u0440\u0438\u0440\u043E\u0434\
  \u0430 Elm \u0437\u043D\u0430\u0447\u0438\u0442, \u0447\u0442\u043E \u0432\u044B\
  \ \u043D\u0435 \u043C\u043E\u0436\u0435\u0442\u0435 \u0433\u0435\u043D\u0435\u0440\
  \u0438\u0440\u043E\u0432\u0430\u0442\u044C \u0441\u043B\u0443\u0447\u0430\u0439\u043D\
  \u044B\u0435 \u0447\u0438\u0441\u043B\u0430 \u043D\u0430\u043F\u0440\u044F\u043C\
  \u0443\u044E, \u043A\u0430\u043A \u044D\u0442\u043E \u0432\u043E\u0437\u043C\u043E\
  \u0436\u043D\u043E \u0432 \u0438\u043C\u043F\u0435\u0440\u0430\u0442\u0438\u0432\
  \u043D\u044B\u0445 \u044F\u0437\u044B\u043A\u0430\u0445.\u2026"
lastmod: '2024-03-13T22:44:44.893640-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0438\u0441\u0442\u043E \u0444\u0443\u043D\u043A\u0446\u0438\u043E\
  \u043D\u0430\u043B\u044C\u043D\u0430\u044F \u043F\u0440\u0438\u0440\u043E\u0434\u0430\
  \ Elm \u0437\u043D\u0430\u0447\u0438\u0442, \u0447\u0442\u043E \u0432\u044B \u043D\
  \u0435 \u043C\u043E\u0436\u0435\u0442\u0435 \u0433\u0435\u043D\u0435\u0440\u0438\
  \u0440\u043E\u0432\u0430\u0442\u044C \u0441\u043B\u0443\u0447\u0430\u0439\u043D\u044B\
  \u0435 \u0447\u0438\u0441\u043B\u0430 \u043D\u0430\u043F\u0440\u044F\u043C\u0443\
  \u044E, \u043A\u0430\u043A \u044D\u0442\u043E \u0432\u043E\u0437\u043C\u043E\u0436\
  \u043D\u043E \u0432 \u0438\u043C\u043F\u0435\u0440\u0430\u0442\u0438\u0432\u043D\
  \u044B\u0445 \u044F\u0437\u044B\u043A\u0430\u0445."
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F \u0441\u043B\u0443\u0447\
  \u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B"
weight: 12
---

## Как это сделать:
Чисто функциональная природа Elm значит, что вы не можете генерировать случайные числа напрямую, как это возможно в императивных языках. Вместо этого используйте модуль `Random` в сочетании с командами. Вот базовый пример, который генерирует случайное целое число от 1 до 100.

Сначала установите модуль `Random` с помощью `elm install elm/random`. Затем импортируйте его в ваш Elm-файл, а также необходимые модули HTML и событий, например так:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Чтобы пример был самодостаточным, добавьте этот шаблон:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Далее, определите **команду** для генерации случайного числа. Это включает в себя настройку типа `Msg` для обработки сгенерированного случайного числа, `Model` для его хранения и функции обновления, чтобы связать все вместе.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

Чтобы инициировать генерацию числа, вы должны отправить сообщение `Generate`, например, через кнопку в вашем интерфейсе:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Случайное число: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Сгенерировать" ]
        ]
```

Когда вы нажмете кнопку "Сгенерировать", на экране отобразится случайное число от 1 до 100.

Этот простой подход можно адаптировать и расширять, используя другие функции в модуле `Random` для создания случайных чисел с плавающей точкой, списков или даже сложных структур данных на основе пользовательских типов, предоставляя обширную площадку для добавления непредсказуемости в ваши Elm-приложения.

Руководство Elm рассказывает об этом гораздо подробнее. Там также есть [пример бросания шестигранного кубика](https://guide.elm-lang.org/effects/random).
