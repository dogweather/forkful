---
title:                "Написання тестів"
date:                  2024-01-19
simple_title:         "Написання тестів"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Що і чому?

Тестування це процес перевірки коду на коректність. Програмісти пишуть тести, щоб забезпечити надійність та якість коду і уникнути помилок.

## Як це робити:

В Haskell тести часто пишуть за допомогою бібліотеки Hspec. Ось приклад простого тесту:

```Haskell
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "abs" $ do
    it "returns the number if given a positive input" $
      abs 1 `shouldBe` 1
      
    it "returns a positive number if given a negative input" $
      abs (-1) `shouldBe` 1
      
    it "returns zero if given zero" $
      abs 0 `shouldBe` 0
```

Виконання цього тесту дасть вам такий результат:

```
abs
  returns the number if given a positive input
  returns a positive number if given a negative input
  returns zero if given zero

Finished in 0.0001 seconds
3 examples, 0 failures
```

## Поглиблений огляд

Тестування в Haskell має довгу історію, Hspec не є єдиною опцією. QuickCheck - інструмент, що дозволяє автоматично генерувати випадкові дані для тестів. Для тестування продуктивності можна використовувати Criterion. Hspec працює на основі BDD (Behavior Driven Development), тобто фокусується на поведінці коду замість його імплементації.

## Дивись також

- [Hspec User's Manual](https://hspec.github.io/)
- [QuickCheck на Hackage](https://hackage.haskell.org/package/QuickCheck)
- [Criterion на Hackage](https://hackage.haskell.org/package/criterion)
