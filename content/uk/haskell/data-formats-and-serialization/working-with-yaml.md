---
title:                "Робота з YAML"
aliases: - /uk/haskell/working-with-yaml.md
date:                  2024-02-03T19:25:47.304027-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
