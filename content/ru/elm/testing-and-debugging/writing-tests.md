---
title:                "Написание тестов"
aliases: - /ru/elm/writing-tests.md
date:                  2024-01-29T00:05:43.557804-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Написание тестов означает создание кода, который проверяет, работает ли ваш основной код как ожидается. Программисты тестируют, чтобы заранее выявить ошибки, обеспечить функциональность и сделать будущие изменения менее рискованными.

## Как это сделать:
Elm использует `elm-test` для написания тестов. Вот небольшой тест для функции `add`, которая суммирует два числа:

```Elm
import Expect
import Test exposing (..)
import AddingModule exposing (add)

suite : Test
suite =
    describe "AddingModule"
        [ test "проверка функции add" <|
            \_ -> Expect.equal (add 1 2) 3
        ]

-- Чтобы запустить тесты, используйте следующую команду:
-- elm-test
```

Если `add` работает правильно, вывод будет следующим:

```
ПРОГОН ТЕСТА ПРОЙДЕН

Продолжительность: 42 мс
Пройдено:   1
Не пройдено:   0
```

## Глубокое погружение
Фреймворк для тестирования в Elm, `elm-test`, предоставляет быстрый и надежный способ написания модульных тестов. Он поощряет разработку, управляемую тестами (TDD - Test-Driven Development). До `elm-test` существовали альтернативы, такие как `elm-check`, но они были не так интегрированы. С точки зрения реализации, `elm-test` использует чистые функции без побочных эффектов, что идеально соответствует архитектуре Elm.

## Смотрите также
- Официальная документация по тестированию в Elm: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Статья о шаблонах тестирования в Elm: https://elmprogramming.com/testing.html
- Пакет `elm-test` на GitHub: https://github.com/elm-explorations/test
