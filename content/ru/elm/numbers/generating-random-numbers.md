---
title:                "Генерация случайных чисел"
date:                  2024-02-27T22:51:03.218398-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-27, dogweather, edited and tested
  - 2024-02-27, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Генерация случайных чисел в Elm включает использование модуля `Random` для создания псевдослучайных чисел, которые пригодятся для различных задач, таких как игры, симуляции, а также как часть алгоритмов, требующих стохастических процессов. Эта возможность позволяет разработчикам добавлять непредсказуемость и разнообразие в свои приложения, повышая пользовательский опыт и функциональность.

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
