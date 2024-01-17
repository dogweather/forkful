---
title:                "Написання тестів"
html_title:           "Haskell: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# Що та чому?

Писання тестів - це процес створення коду, який перевіряє працездатність програми, щоб забезпечити її якість та коректність. Програмісти роблять це, щоб переконатися, що їх код працює правильно та відповідає очікуванням.

# Як?

```Haskell
import Test.HUnit

-- Приклад тестування додавання
addTest :: Test
addTest = TestCase (assertEqual "Додавання 2 + 3" 5 (2 + 3))

-- Запуск усіх тестів
runTests :: IO Counts
runTests = runTestTT $ TestList [addTest]

-- Вивід результату тестів у консолі
main :: IO ()
main = do
  Counts cases tried errors failures <- runTests
  putStrLn $ "Успішно: " ++ show (cases - (errors + failures))
  putStrLn $ "Неуспішно: " ++ show (errors + failures)
```

Вивід:
```
Успішно: 1
Неуспішно: 0
```

# Вглиб

Писання тестів є важливою практикою у розробці програмного забезпечення, яка стала поширеною під час розвитку методологій Agile та Test-Driven Development (TDD). Існує багато альтернативних бібліотек для тестування в Haskell, таких як Hspec та QuickCheck. Детальніше про ці бібліотеки можна дізнатися на їх офіційних веб-сайтах.

Також важливо розуміти, що при написанні тестів потрібно враховувати різні сценарії та крайні випадки, щоб переконатися, що програма працює на всіх можливих вхідних даних.

# Дивись також

- [Офіційна документація з Test.HUnit](https://hackage.haskell.org/package/HUnit/docs/Test-HUnit.html)
- [Офіційна документація з Hspec](https://hspec.github.io/)
- [Офіційна документація з QuickCheck](https://hackage.haskell.org/package/QuickCheck)