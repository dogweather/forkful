---
title:                "Письмо тестів"
aliases:
- /uk/haskell/writing-tests.md
date:                  2024-02-03T19:31:06.291428-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Написання тестів на Haskell полягає в тому, щоб забезпечити роботу ваших функцій так, як очікується, за допомогою автоматизованих перевірок. Програмісти роблять це, щоб виявляти помилки на ранньому етапі, полегшити рефакторинг і задокументувати поведінку, роблячи кодову базу більш обслуговуваною і масштабованою.

## Як:

Haskell підтримує кілька тестових фреймворків, але два популярні - це `Hspec` і `QuickCheck`. Hspec дозволяє вам визначати зрозумілі для людини специфікації для вашого коду, тоді як QuickCheck дозволяє автоматично генерувати тести, описуючи властивості, які ваш код повинен задовольняти.

### Використання Hspec

Спочатку додайте `hspec` до конфігурації вашого інструмента збірки (наприклад, у файл `stack.yaml` або `cabal`). Потім імпортуйте `Test.Hspec` та напишіть тести як специфікації:

```haskell
-- файл: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "додає два числа" $
    add 1 2 `shouldBe` 3

  it "повертає перше число при додаванні нуля" $
    add 5 0 `shouldBe` 5
```

Потім запустіть ваші тести за допомогою вашого інструменту збірки, результат може виглядати так:

```
MyLib.add
  - додає два числа
  - повертає перше число при додаванні нуля

Завершено за 0.0001 секунди
2 приклади, 0 невдач
```

### Використання QuickCheck

З QuickCheck ви виражаєте властивості, які ваші функції мають задовольняти. Додайте `QuickCheck` до конфігурації вашого проекту, потім імпортуйте його:

```haskell
-- файл: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

Запуск цих тестів автоматично генерує вхідні дані для перевірки зазначених властивостей:

```
+++ OK, пройшло 100 тестів.
+++ OK, пройшло 100 тестів.
```

У прикладах як з Hspec, так і з QuickCheck, набори тестів служать як виконувана документація, яка може автоматично перевірити правильність вашого коду.
