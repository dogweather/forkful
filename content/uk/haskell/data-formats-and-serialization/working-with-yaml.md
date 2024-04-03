---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:47.304027-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: Haskell \u043D\u0435\
  \ \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0457\
  \ \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438 \u0434\u043B\u044F \u043E\
  \u0431\u0440\u043E\u0431\u043A\u0438 YAML, \u0430\u043B\u0435 \u0432\u0438 \u043C\
  \u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u0432\u0430\u0442\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\
  \u0456 \u044F\u043A `yaml` \u0442\u0430 `aeson`, \u0434\u043B\u044F \u0440\u043E\
  \u0437\u0431\u043E\u0440\u0443\u2026"
lastmod: '2024-03-13T22:44:49.394740-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438\
  \ \u0434\u043B\u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438 YAML, \u0430\u043B\
  \u0435 \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0441\u0442\u043E\
  \u0440\u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0438, \u0442\u0430\u043A\u0456 \u044F\u043A `yaml` \u0442\u0430 `aeson`, \u0434\
  \u043B\u044F \u0440\u043E\u0437\u0431\u043E\u0440\u0443 \u0442\u0430 \u0433\u0435\
  \u043D\u0435\u0440\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445 YAML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

## Як робити:
Haskell не має вбудованої підтримки для обробки YAML, але ви можете використовувати сторонні бібліотеки, такі як `yaml` та `aeson`, для розбору та генерації даних YAML. Ось як ви можете розпочати:

### Читання YAML
Спочатку, додайте пакет `yaml` до залежностей вашого проекту. Потім, ви можете використовувати наступний приклад для розбору простого документа YAML:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Приклад даних YAML
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- Визначаємо структуру даних, яка відповідає документу YAML
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "Помилка при розборі YAML: " ++ show err
    Right person -> print person
```
Зразок виводу для вище наведеного коду може виглядати так:
```
Person {name = "John Doe", age = 30}
```

### Запис YAML
Для генерації YAML з структур даних Haskell можна використовувати функціонал кодування пакету `yaml`, як показано нижче:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Використовуючи структуру даних Person з попереднього прикладу

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
Вивід цієї програми буде рядком, відформатованим у YAML:
```
name: Jane Doe
age: 25
```

Ці приклади повинні стати відправною точкою для роботи з YAML у Haskell. Залежно від ваших потреб, можливо, вам захочеться дослідити більш розширені можливості та опції, які пропонують ці бібліотеки.
