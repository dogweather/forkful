---
title:                "Написание тестов"
aliases:
- /ru/haskell/writing-tests.md
date:                  2024-01-29T00:06:11.773292-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Написание тестов позволяет проверить, выполняет ли код свои функции. Тестирование выявляет ошибки, помогает поддерживать код в актуальном состоянии и гарантирует, что изменения не приведут к поломкам.

## Как это сделать:

Haskell использует HUnit для базовых модульных тестов и QuickCheck для тестов, основанных на свойствах. Вот быстрый пример с использованием HUnit:

```haskell
import Test.HUnit

testList :: Test
testList = TestList [TestCase (assertEqual "Должно сложить числа" 4 (2 + 2)),
                     TestCase (assertEqual "Должно вычесть числа" 0 (2 - 2))]

main :: IO Counts
main = runTestTT testList
```

При запуске он покажет:

```
Cases: 2  Tried: 2  Errors: 0  Failures: 0
```

Пример с использованием QuickCheck:

```haskell
import Test.QuickCheck

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

main :: IO ()
main = quickCheck prop_RevRev
```

Примерный результат выполнения:

```
+++ OK, пройдено 100 тестов.
```

## Подробнее

Тестирование началось с раннего программирования, но получило серьезное развитие с ростом популярности TDD в 2000-х. Чистые функции в Haskell делают его отличным выбором для тестирования. Альтернативы HUnit/QuickCheck включают doctest и Hedgehog. HUnit похож на JUnit из Java. QuickCheck автоматизирует генерацию тестовых случаев, проверяя свойства, которые вы определяете.

## См. также

- Документация HUnit: [http://hackage.haskell.org/package/HUnit](http://hackage.haskell.org/package/HUnit)
- QuickCheck на Hackage: [http://hackage.haskell.org/package/QuickCheck](http://hackage.haskell.org/package/QuickCheck)
- Введение в тестирование на Haskell: [https://hspec.github.io/](https://hspec.github.io/)
- "Haskell в реальных задачах" Брайан О'Салливан, Дон Стюарт, и Джон Гоерзен: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)
