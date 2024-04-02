---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:47.304027-07:00
description: "YAML, \u0441\u043A\u043E\u0440\u043E\u0447\u0435\u043D\u043D\u044F \u0432\
  \u0456\u0434 \"YAML Ain't Markup Language\", \u0454 \u0437\u0440\u0443\u0447\u043D\
  \u0438\u043C \u0434\u043B\u044F \u043B\u044E\u0434\u0435\u0439 \u0441\u0442\u0430\
  \u043D\u0434\u0430\u0440\u0442\u043E\u043C \u0441\u0435\u0440\u0456\u0430\u043B\u0456\
  \u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\u043A\u0438\
  \u0439 \u043C\u043E\u0436\u0435 \u0431\u0443\u0442\u0438 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u0430\u043D\u0438\u0439 \u0443 \u0432\u0441\u0456\u0445\
  \ \u043C\u043E\u0432\u0430\u0445\u2026"
lastmod: '2024-03-13T22:44:49.394740-06:00'
model: gpt-4-0125-preview
summary: "YAML, \u0441\u043A\u043E\u0440\u043E\u0447\u0435\u043D\u043D\u044F \u0432\
  \u0456\u0434 \"YAML Ain't Markup Language\", \u0454 \u0437\u0440\u0443\u0447\u043D\
  \u0438\u043C \u0434\u043B\u044F \u043B\u044E\u0434\u0435\u0439 \u0441\u0442\u0430\
  \u043D\u0434\u0430\u0440\u0442\u043E\u043C \u0441\u0435\u0440\u0456\u0430\u043B\u0456\
  \u0437\u0430\u0446\u0456\u0457 \u0434\u0430\u043D\u0438\u0445, \u044F\u043A\u0438\
  \u0439 \u043C\u043E\u0436\u0435 \u0431\u0443\u0442\u0438 \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u0430\u043D\u0438\u0439 \u0443 \u0432\u0441\u0456\u0445\
  \ \u043C\u043E\u0432\u0430\u0445\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 YAML"
weight: 41
---

## Що і чому?

YAML, скорочення від "YAML Ain't Markup Language", є зручним для людей стандартом серіалізації даних, який може бути використаний у всіх мовах програмування. Програмісти часто використовують YAML у файлах конфігурації та для обміну даними між мовами завдяки його зрозумілості та простій структурі.

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
