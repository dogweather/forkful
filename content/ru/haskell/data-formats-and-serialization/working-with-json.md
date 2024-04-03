---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:01.830348-07:00
description: "\u041A\u0430\u043A: \u0412 Haskell \u043C\u044B \u0440\u0430\u0431\u043E\
  \u0442\u0430\u0435\u043C \u0441 JSON \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0438 `aeson`. \u0414\u043B\u044F \u043D\u0430\u0447\u0430\u043B\
  \u0430 \u0438\u043C\u043F\u043E\u0440\u0442\u0438\u0440\u0443\u0439\u0442\u0435\
  \ \u0435\u0435 \u0438 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0438\u0442\u0435\
  \ \u0442\u0438\u043F, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u0441\u043E\u043E\
  \u0442\u0432\u0435\u0442\u0441\u0442\u0432\u0443\u0435\u0442 \u043E\u0436\u0438\u0434\
  \u0430\u0435\u043C\u043E\u0439\u2026"
lastmod: '2024-03-13T22:44:45.172078-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Haskell \u043C\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0435\u043C\
  \ \u0441 JSON \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u043D\u0438\u0435\u043C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438\
  \ `aeson`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
