---
title:                "Генерація випадкових чисел"
date:                  2024-02-27T22:50:22.791884-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-27, dogweather, edited and tested
  - 2024-02-27, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Генерація випадкових чисел у Elm включає використання модуля `Random` для створення псевдовипадкових чисел, які стають у пригоді для різноманітних завдань, таких як ігри, симуляції, а також як частина алгоритмів, що вимагають стохастичних процесів. Ця можливість дозволяє розробникам додавати непередбачуваність і різноманіття до своїх програм, покращуючи досвід користувача та функціональність.

## Як це зробити:
Чиста функціональна природа Elm означає, що ви не можете генерувати випадкові числа безпосередньо, як ви могли б це робити в імперативних мовах. Натомість, ви використовуєте модуль `Random` у поєднанні з командами. Ось простий приклад, який генерує випадкове ціле число між 1 і 100.

Спочатку встановіть модуль `Random` за допомогою `elm install elm/random`. Потім імпортуйте його до вашого файлу Elm, разом з необхідними модулями HTML і подій, отак:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Для створення цього самодостатнього прикладу ви можете додати цей шаблонний код:
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

Далі, визначте **команду** для генерації випадкового числа. Це включає налаштування типу `Msg` для обробки випадкового числа після його генерації, `Model` для його зберігання, а також функцію оновлення, щоб усе це зв'язати разом.
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

Для активації генерації числа, ви б надсилали повідомлення `Generate`, наприклад, через кнопку у вашому представленні:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Випадкове число: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Генерувати" ]
        ]
```

Коли ви натискаєте на кнопку "Генерувати", буде відображено випадкове число між 1 і 100.

Цей простий підхід можна адаптувати та розширити, використовуючи інші функції модуля `Random` для створення випадкових дробів, списків або навіть складних структур даних на основі користувацьких типів, надаючи величезний простір для додавання непередбачуваності до ваших програм на Elm.

Посібник Elm входить у набагато більше подробиць. Там також є [приклад кидання шестигранного кубика](https://guide.elm-lang.org/effects/random).
