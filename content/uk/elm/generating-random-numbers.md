---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?

Генерація випадкових чисел - це процес створення чисел, які не можна передбачити краще ніж випадковим вибором. Програмісти роблять це для запуску випадкових сценаріїв у своїх програмах, типічно для тестування або для створення елементів гри.

## Як застосувати:

Ми можемо генерувати випадкове число в Elm використовуючи модуль `Random`. Ось приклад:

```elm
import Random

type alias Model =
    { generator : Random.Generator Int }

initialModel : Model
initialModel =
    { generator = Random.int 0 100 }

update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model
    , Random.generate NewRandomNumber model.generator
    )
```

При цьому, випадкове число буде згенероване у межах від 0 до 100. 

## Поглиблено:

Генерація випадкових чисел має глибокі корені в історії обчислювальної техніки. В основу Elm вбудована високоякісна бібліотека для генерації випадкових чисел, що використовує генератор випадкових чисел Мерсена Твістера.

Альтернативами модулю `Random` в Elm є `elm-random-pcg` або `elm-random-extra`, але вони мають свої плюси і мінуси.

Не забувайте, що випадкові числа в Elm є функЦіонульний тип (`Generator`), тому вони не генеруються прямо, а вміщуються в команду (`Cmd`), яка потім виконується Elm runtime.

## Дивіться ще: 

1. [Random - Elm package](https://package.elm-lang.org/packages/elm/random/latest/)
2. [Generating random numbers in Elm](https://medium.com/@julianjelfs/generating-random-numbers-in-elm-58495e9fff9f)