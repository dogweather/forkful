---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:06.291428-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 Haskell \u043F\u043E\u043B\u044F\u0433\u0430\u0454\
  \ \u0432 \u0442\u043E\u043C\u0443, \u0449\u043E\u0431 \u0437\u0430\u0431\u0435\u0437\
  \u043F\u0435\u0447\u0438\u0442\u0438 \u0440\u043E\u0431\u043E\u0442\u0443 \u0432\
  \u0430\u0448\u0438\u0445 \u0444\u0443\u043D\u043A\u0446\u0456\u0439 \u0442\u0430\
  \u043A, \u044F\u043A \u043E\u0447\u0456\u043A\u0443\u0454\u0442\u044C\u0441\u044F\
  , \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0430\u0432\
  \u0442\u043E\u043C\u0430\u0442\u0438\u0437\u043E\u0432\u0430\u043D\u0438\u0445 \u043F\
  \u0435\u0440\u0435\u0432\u0456\u0440\u043E\u043A. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.365081-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 Haskell \u043F\u043E\u043B\u044F\u0433\u0430\u0454\
  \ \u0432 \u0442\u043E\u043C\u0443, \u0449\u043E\u0431 \u0437\u0430\u0431\u0435\u0437\
  \u043F\u0435\u0447\u0438\u0442\u0438 \u0440\u043E\u0431\u043E\u0442\u0443 \u0432\
  \u0430\u0448\u0438\u0445 \u0444\u0443\u043D\u043A\u0446\u0456\u0439 \u0442\u0430\
  \u043A, \u044F\u043A \u043E\u0447\u0456\u043A\u0443\u0454\u0442\u044C\u0441\u044F\
  , \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0430\u0432\
  \u0442\u043E\u043C\u0430\u0442\u0438\u0437\u043E\u0432\u0430\u043D\u0438\u0445 \u043F\
  \u0435\u0440\u0435\u0432\u0456\u0440\u043E\u043A. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438\u2026"
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
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
