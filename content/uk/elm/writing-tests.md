---
title:                "Написання тестів"
html_title:           "Elm: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Для чого

Коли ви пишете програми в Elm, тестування є важливою частиною процесу розробки. Тести допомагають вам переконатися, що ваш код працює як очікувано, а також зберігають вас від непотрібної роботи вручну.

## Як

Написання тестів може здатися складним, але з Elm це легше, ніж ви можете подумати. Давайте подивимося на приклад тесту для функції `sumNumbers`:

```Elm
sumNumbers : Int -> Int -> Int
sumNumbers x y =
    x + y


testSumNumbers : Test
testSumNumbers =
    describe "sumNumbers"
        [ test "Returns correct sum" <|
            \_ ->
                Expect.equal (sumNumbers 5 10) 15
        ]
```

Щоб написати тест у Elm, спочатку нам потрібно визначити функцію та описати тестову сценарію за допомогою функції `describe`. Далі ви можете використовувати функцію `test` для створення конкретного тесту та перевірки його результату за допомогою функції `Expect`.

Щоб запустити цей тест у своїй програмі, ви можете використовувати модуль `Test.Runner` та функцію `run`:

```Elm
import Test.Runner


main : Program Never
main =
    Test.Runner.run testSumNumbers

```

Якщо ваша функція працює як очікувано, ви побачите наступне повідомлення:

```
Passed 1 test.
Test had no failures.
```

## Глибокий занурення

У Elm є багато корисних функцій, які можна використовувати для тестування вашого коду. Наприклад, ви можете використовувати функцію `Expect.equal` для порівняння значень, або `Expect.fails` для перевірки, що певна функція спрацьовує виключно. Крім того, у Elm є можливість створювати собі власні спеціальні функції для тестування.

Не бійтесь експерементувати та вдосконалювати свої навики у написанні тестів у Elm. Це допоможе вам створювати більш якісний та стабільний код.

## Дивись також

- [Офіційна документація Elm по написанню тестів](https://guide.elm-lang.org/testing/)
- [Простий урок з тестування у Elm](https://medium.com/@shreks7/unit-testing-a-simple-calculator-in-elm-a58241550da7)
- [Бібліотека Dahlia/elm-test для тестування у Elm](https://package.elm-lang.org/packages/dahlia/elm-test/latest/)