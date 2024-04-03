---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:11.773292-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\
  \u0442\u043E\u0432 \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\u0442 \u043F\
  \u0440\u043E\u0432\u0435\u0440\u0438\u0442\u044C, \u0432\u044B\u043F\u043E\u043B\
  \u043D\u044F\u0435\u0442 \u043B\u0438 \u043A\u043E\u0434 \u0441\u0432\u043E\u0438\
  \ \u0444\u0443\u043D\u043A\u0446\u0438\u0438. \u0422\u0435\u0441\u0442\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u0435 \u0432\u044B\u044F\u0432\u043B\u044F\u0435\u0442\
  \ \u043E\u0448\u0438\u0431\u043A\u0438, \u043F\u043E\u043C\u043E\u0433\u0430\u0435\
  \u0442 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\u0442\u044C\
  \ \u043A\u043E\u0434 \u0432 \u0430\u043A\u0442\u0443\u0430\u043B\u044C\u043D\u043E\
  \u043C \u0441\u043E\u0441\u0442\u043E\u044F\u043D\u0438\u0438 \u0438\u2026"
lastmod: '2024-03-13T22:44:45.140307-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\
  \u0442\u043E\u0432 \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\u0442 \u043F\
  \u0440\u043E\u0432\u0435\u0440\u0438\u0442\u044C, \u0432\u044B\u043F\u043E\u043B\
  \u043D\u044F\u0435\u0442 \u043B\u0438 \u043A\u043E\u0434 \u0441\u0432\u043E\u0438\
  \ \u0444\u0443\u043D\u043A\u0446\u0438\u0438."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

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
