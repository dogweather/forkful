---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:06.291428-07:00
description: "\u042F\u043A: Haskell \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\
  \u0454 \u043A\u0456\u043B\u044C\u043A\u0430 \u0442\u0435\u0441\u0442\u043E\u0432\
  \u0438\u0445 \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u0456\u0432\
  , \u0430\u043B\u0435 \u0434\u0432\u0430 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\
  \u043D\u0456 - \u0446\u0435 `Hspec` \u0456 `QuickCheck`. Hspec \u0434\u043E\u0437\
  \u0432\u043E\u043B\u044F\u0454 \u0432\u0430\u043C \u0432\u0438\u0437\u043D\u0430\
  \u0447\u0430\u0442\u0438 \u0437\u0440\u043E\u0437\u0443\u043C\u0456\u043B\u0456\
  \ \u0434\u043B\u044F \u043B\u044E\u0434\u0438\u043D\u0438\u2026"
lastmod: '2024-03-13T22:44:49.365081-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u043A\u0456\
  \u043B\u044C\u043A\u0430 \u0442\u0435\u0441\u0442\u043E\u0432\u0438\u0445 \u0444\
  \u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u0456\u0432, \u0430\u043B\u0435\
  \ \u0434\u0432\u0430 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0456 - \u0446\
  \u0435 `Hspec` \u0456 `QuickCheck`."
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

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
