---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:14.933254-07:00
description: "\u041A\u0430\u043A: \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\
  \u043E\u0442\u0430\u0442\u044C \u0441 CSV \u0432 Haskell, \u043C\u043E\u0436\u043D\
  \u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 `cassava`. \u0423\
  \u0441\u0442\u0430\u043D\u043E\u0432\u0438\u0442\u0435 \u0435\u0451, \u0434\u043E\
  \u0431\u0430\u0432\u0438\u0432 `cassava` \u0432 \u0432\u0430\u0448 \u0444\u0430\u0439\
  \u043B `.cabal` \u0438\u043B\u0438 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u044F Stack.\u2026"
lastmod: '2024-03-13T22:44:45.173760-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0442\
  \u044C \u0441 CSV \u0432 Haskell, \u043C\u043E\u0436\u043D\u043E \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A\u0443 `cassava`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как:
Чтобы работать с CSV в Haskell, можно использовать библиотеку `cassava`. Установите её, добавив `cassava` в ваш файл `.cabal` или используя Stack. Вот как декодировать и кодировать данные CSV:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Предположим, мы работаем с этим типом
type Person = (String, Int, Bool)

-- Пример данных CSV
csvData :: BL.ByteString
csvData = "John Doe,30,true\nJane Smith,25,false"

-- Декодирование данных CSV
decodePeople :: BL.ByteString -> Either String (V.Vector Person)
decodePeople = fmap snd . decode NoHeader

-- Кодирование данных в CSV
encodePeople :: V.Vector Person -> BL.ByteString
encodePeople = encode

-- Пример использования
main :: IO ()
main = do
  -- Декодирование
  case decodePeople csvData of
    Left err -> putStrLn err
    Right v -> print v
  
  -- Кодирование
  let people = V.fromList [("Alice", 23, True), ("Bob", 35, False)]
  BL.putStrLn $ encodePeople people
```

Пример вывода:
```plaintext
[("John Doe",30,True),("Jane Smith",25,False)]
"Alice",23,True
"Bob",35,False
```

## Глубокое погружение
Обработка CSV в Haskell эволюционировала. Самые ранние методы включали в себя ручной разбор строк, что было подвержено ошибкам. `cassava` предлагает типобезопасный парсинг, опираясь на сильную систему типов Haskell. К альтернативам относится пакет `csv`, но `cassava` более эффективна и гибка. С точки зрения реализации, `cassava` использует потоки для эффективности памяти и скорости, что важно при работе с большими наборами данных.

## Смотрите также
- Библиотека `cassava` на Hackage: https://hackage.haskell.org/package/cassava
- Библиотека Haskell's ByteString для обработки бинарных данных: https://hackage.haskell.org/package/bytestring
- Руководство по библиотеке Vector, для эффективных списков: https://hackage.haskell.org/package/vector
