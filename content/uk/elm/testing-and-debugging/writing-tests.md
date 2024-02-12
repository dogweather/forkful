---
title:                "Письмо тестів"
aliases:
- /uk/elm/writing-tests.md
date:                  2024-02-03T19:31:00.548951-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Написання тестів в Elm включає створення тестових випадків для перевірки правильності вашого коду на Elm, забезпечуючи його очікувану поведінку. Програмісти роблять це, щоб виявити помилки на ранніх стадіях, полегшити обслуговування та покращити якість та надійність своїх додатків.

## Як:

Elm використовує пакет `elm-explorations/test` для написання модульних та фазз-тестів. Почніть з додавання пакету до вашого проєкту:

```elm
elm install elm-explorations/test
```

Створіть тестовий файл, скажімо `tests/ExampleTest.elm`, та імпортуйте тестові модулі. Ось простий тест, який перевіряє функцію `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Проста функція додавання"
        [ test "Додавання 2 і 3 дає 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

Для запуску ваших тестів вам буде потрібно `elm-test`:

```shell
npm install -g elm-test
elm-test
```

Це скомпілює ваші тести та виведе результати в терміналі. Для вищезазначеного прикладу вивід повинен бути приблизно наступним:

```
ТЕСТ ВИКОНАНО УСПІШНО

Тривалість: 42 мс
Пройдено:   1
Помилок:   0
```

Для більш складного прикладу, скажімо ви хочете протестувати функцію `add` фазз-тестом, щоб переконатися, що вона правильно обробляє широкий діапазон вхідних цілих чисел. Ви могли б змінити ваш `ExampleTest.elm` таким чином:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Тестування add з фаззінгом"
        [ fuzz int "Фазз тестування add з випадковими цілими числами" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Запустіть `elm-test` знову, щоб побачити фазз-тести на дії. Результат буде змінюватися з випадковим вводом, але успішні тести вкажуть на відсутність помилок:

```
ТЕСТ ВИКОНАНО УСПІШНО

Тривалість: 183 мс
Пройдено:   100
Помилок:   0
```

Ці приклади показують, як писати та виконувати прості модульні та фазз-тести в Elm, використовуючи пакет `elm-explorations/test`. Тестування є життєво важливою частиною процесу розробки, допомагаючи забезпечити надійність ваших додатків Elm і підтримувати їх високу якість.
