---
title:                "Извлечение подстрок"
date:                  2024-01-28T23:57:55.463045-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Извлечение подстроки подразумевает выборка конкретных частей из строки. Программисты делают это, чтобы изолировать, манипулировать или анализировать фрагменты текстовых данных.

## Как:

В Elm это делается легко. Для начала используем `String.slice`:

```Elm
import String exposing (slice)

fullText : String
fullText = "Hello, Elm world!"

-- Извлечение "Elm"
substring : String
substring = slice 7 10 fullText

-- Вывод: "Elm"
```

Теперь давайте будем немного более динамичны с `String.left` и `String.right`:

```Elm
import String exposing (left, right)

-- Получение первых 5 символов
leftString : String
leftString = left 5 fullText

-- Вывод: "Hello"

-- Получение последних 5 символов
rightString : String
rightString = right 5 fullText

-- Вывод: "orld!"
```

## Погружаемся глубже

Исторически, извлечение подстроки существует столько же, сколько и программирование. В Elm, как и в других функциональных языках, функции манипуляции строками являются неизменяемыми - они возвращают новые строки, а не изменяют оригинал.

Существуют альтернативы, такие как `String.dropLeft` и `String.dropRight`. Они удаляют символы с любого конца строки:

```Elm
import String exposing (dropLeft, dropRight)

-- Удаление первых 7 символов
droppedLeftString : String
droppedLeftString = dropLeft 7 fullText

-- Вывод: "Elm world!"

-- Удаление последних 6 символов
droppedRightString : String
droppedRightString = dropRight 6 fullText

-- Вывод: "Hello, Elm"
```

С точки зрения реализации, эти функции встроены в стандартную библиотеку Elm и поддерживают Юникод, хотя с парами замещения Юникода и комбинированными символами есть свои особенности.

## Смотрите также

- Документация модуля `String` в Elm: https://package.elm-lang.org/packages/elm/core/latest/String
- Руководство Elm по строкам: https://guide.elm-lang.org/strings/
- MDN Web Docs о Юникоде: https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/String/charAt
