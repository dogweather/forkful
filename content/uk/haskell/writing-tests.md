---
title:    "Haskell: Написання тестів"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Написання тестів є важливим кроком у створенні високоякісного програмного продукту. Це дає вам впевненість у тому, що ваш код працює правильно та захищений від можливих помилок у майбутньому.

## Як

```Haskell
import Test.HUnit

-- Приклад тесту для функції додавання
testAdd = TestCase (assertEqual "Додавання двох чисел повинно давати правильний результат" (add 2 3) 5)

-- Запуск тестів та виведення результатів у консоль
runTestTT testAdd
-- Вивід: Cases: 1  Tried: 1  Errors: 0  Failures: 0

```

## Глибше вдивимося

Написання тестів дозволяє вам швидко перевіряти правильність роботи вашого коду та виявляти проблеми на ранніх етапах розробки. Це також допомагає уникнути збоїв у програмі та забезпечує більшість проблем під час тестування, що зекономить час та зусилля у майбутньому.

## Дивись також

- [Туторіал по написанню тестів у Haskell](https://wiki.haskell.org/Tutorials/Testing)
- [Оfficial документація. модуль тестування HUnit] (https://hackage.haskell.org/package/HUnit)
- [Стаття про засоби тестування у Haskell](https://ucancode.me/blog/2019/02/05/testing-haskell-code-with-quickcheck-and-qa-tools/)