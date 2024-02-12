---
title:                "Работа с JSON"
aliases: - /ru/haskell/working-with-json.md
date:                  2024-01-29T00:04:01.830348-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

JSON (JavaScript Object Notation) — это текстовый формат данных для хранения и передачи данных. Программисты используют его, потому что он легкий, легко читаемый/записываемый и независимый от языка.

## Как:

В Haskell мы работаем с JSON с использованием библиотеки `aeson`. Для начала импортируйте ее и определите тип, который соответствует ожидаемой структуре JSON.

```Haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

-- Предположим, у нас есть объект JSON с "name" и "age"

data Person = Person 
  { name :: String
  , age  :: Int
  } deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person

-- Разбор строки JSON
main :: IO ()
main = do
  let jsonString = "{\"name\":\"John\", \"age\":30}"
  let maybePerson = decode jsonString :: Maybe Person
  case maybePerson of
    Nothing -> putStrLn "Ошибка при разборе JSON."
    Just person -> print person
```

Вывод:
```
Person {name = "John", age = 30}
```

## Погружение

- **История**: Дизайн JSON был вдохновлен подмножеством синтаксиса JavaScript, и он впервые получил популярность как простая альтернатива XML.
- **Альтернативы**: Хотя JSON является королем для веб-API, в зависимости от контекста и требований могут быть выбраны альтернативы, такие как XML, YAML или даже Protocol Buffers.
- **Детали реализации**: `aeson` использует систему типов Haskell для соответствия структур JSON типам Haskell. Разбор выполняется через классы типов, такие как `FromJSON`, и кодирование через `ToJSON`.

## Смотрите также

- Документация пакета `aeson`: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
- Реальные веб-API JSON для практики: [https://jsonplaceholder.typicode.com/](https://jsonplaceholder.typicode.com/)
- Спецификация JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
