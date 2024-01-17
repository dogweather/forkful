---
title:                "Генерування випадкових чисел"
html_title:           "Elm: Генерування випадкових чисел"
simple_title:         "Генерування випадкових чисел"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що & Чому?

Генерування випадкових чисел - це процес створення чисел за випадковим алгоритмом. Програмісти використовують цю функцію для створення рандомних змінних та елементів для своїх програм.

## Як:

```Elm
import Random exposing (..)

randomInt : Int
randomInt =
  generator_range 1 10

-- Вивід:
-- 4
```

```Elm
import Random exposing (..)

randomFloat : Float
randomFloat =
  generator_float 0 1

-- Вивід:
-- 0.5328805454338112
```

```Elm
import Random exposing (..)

randomBool : Bool
randomBool =
  generator_bool

-- Вивід:
-- True
```

## Глибша інформація:

Генерування випадкових чисел давно застосовується в програмуванні, особливо в області криптографії та статистики. Завдяки цьому функціонуванню, програмісти можуть створювати випадкову поведінку та випадкові дані, необхідні для дослідження та тестування.

Існують альтернативні способи генерування випадкових чисел, такі як використання системних функцій або сторонніх бібліотек. Однак, у порівнянні з іншими методами, Elm надає просту та ефективну функцію для генерування випадкових чисел.

## Див. також:

- [Elm документація про генерування випадкових чисел](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [Стаття про генерування випадкових чисел на сайті "Habr"](https://m.habr.com/ru/company/mailru/blog/568266/)