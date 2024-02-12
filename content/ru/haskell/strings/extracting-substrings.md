---
title:                "Извлечение подстрок"
aliases: - /ru/haskell/extracting-substrings.md
date:                  2024-01-28T23:58:08.641304-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Извлечение подстрок означает выделение конкретных частей строки. Программисты делают это для изоляции данных, их очистки или работы с частями вместо целого.

## Как:

В Haskell вы можете извлекать и редактировать строки с помощью встроенных функций, таких как `take`, `drop` и `substring` (из `Data.Text`).

```haskell
import Data.Text (Text, pack, unpack, take, drop)

-- Наша примерная строка
let exampleStr = "Haskell makes sense!"

-- Взятие первых 7 символов
print $ unpack (take 7 (pack exampleStr)) -- "Haskell"

-- Пропуск первых 8 символов
print $ unpack (drop 8 (pack exampleStr)) -- "makes sense!"

-- Пользовательская функция для извлечения подстроки по позиции и длине
substring :: Int -> Int -> Text -> Text
substring start length = take length . drop start

-- Извлечение "makes" (начиная с позиции 8, длина 5)
print $ unpack (substring 8 5 (pack exampleStr)) -- "makes"
```

Пример результатов:
```
"Haskell"
"makes sense!"
"makes"
```

## Подробнее

Извлечение подстрок является частью Haskell уже много лет. Сначала это осуществлялось на основе списков, поскольку строки являются списками символов. Производительность была не очень. Затем появился `Data.Text`, предлагающий эффективные операции со строками.

Альтернативы включают операции со списками, регулярные выражения и библиотеки анализа. Операции со списками проще, но медленнее для больших строк. Регулярные выражения мощные, но избыточные для простых задач. Библиотеки анализа предназначены для сложного анализа, но также могут обрабатывать подстроки.

Реализация пользовательской функции подстроки в Haskell проста с использованием `take` и `drop` из `Data.Text`, обеспечивая более быструю обработку строк, чем операции на основе списков.

## Смотрите также

- Документация модуля `Data.Text`: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html
- Учим Haskell ради большого блага! для легкого начала работы со строками в Haskell: http://learnyouahaskell.com/starting-out#immutability
- Haskell в реальном мире для практических примеров использования: http://book.realworldhaskell.org/read/
- Вики Haskell для понимания мнений сообщества: https://wiki.haskell.org/How_to_work_with_strings
