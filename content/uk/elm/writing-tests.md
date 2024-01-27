---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Тести - це скрипти, які перевіряють, що ваш код працює правильно. Програмісти пишуть тести, щоб запобігти помилкам, спростити рефакторинг і забезпечити що новий код не руйнує старий.

## How to: (Як це робити:)
```Elm
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Test.Runner.Node

suite : Test
suite =
    describe "Арифметичні операції"
        [ test "Додавання працює коректно" <| \_ ->
            Expect.equal (2 + 2) 4
        , test "Віднімання працює коректно" <| \_ ->
            Expect.equal (5 - 3) 2
        ]

-- Для запуску тестів використовуйте таку команду:
-- elm-test
```
Висновок тестів:

```
Виконується 2 тести. Всі пройшли успішно.
```

## Deep Dive (Поглиблений Аналіз)
В Elm, тестування починалося з простих функцій, що порівнюють вирази, і з часом розвинулося до більш комплексних бібліотек з `elm-test`. Альтернативою може бути інтеграція з JavaScript через порти для використання таких інструментів як Jasmine або Mocha. Однак, `elm-test` залишається стандартним вибором для Elm-розробників завдяки тому, що він глибоко інтегрований з мовою та слідує її філософії.

## See Also (Дивіться також)
- [elm-test пакет на GitHub](https://github.com/elm-explorations/test)
- [Elm реактор для інтерактивної розробки](https://elm-lang.org/blog/compiler-errors-for-humans)
