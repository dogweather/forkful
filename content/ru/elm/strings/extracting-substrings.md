---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:55.463045-07:00
description: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\
  \u0434\u0441\u0442\u0440\u043E\u043A\u0438 \u043F\u043E\u0434\u0440\u0430\u0437\u0443\
  \u043C\u0435\u0432\u0430\u0435\u0442 \u0432\u044B\u0431\u043E\u0440\u043A\u0430\
  \ \u043A\u043E\u043D\u043A\u0440\u0435\u0442\u043D\u044B\u0445 \u0447\u0430\u0441\
  \u0442\u0435\u0439 \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\
  \u044E\u0442 \u044D\u0442\u043E, \u0447\u0442\u043E\u0431\u044B \u0438\u0437\u043E\
  \u043B\u0438\u0440\u043E\u0432\u0430\u0442\u044C, \u043C\u0430\u043D\u0438\u043F\
  \u0443\u043B\u0438\u0440\u043E\u0432\u0430\u0442\u044C \u0438\u043B\u0438 \u0430\
  \u043D\u0430\u043B\u0438\u0437\u0438\u0440\u043E\u0432\u0430\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:44.880875-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\
  \u0434\u0441\u0442\u0440\u043E\u043A\u0438 \u043F\u043E\u0434\u0440\u0430\u0437\u0443\
  \u043C\u0435\u0432\u0430\u0435\u0442 \u0432\u044B\u0431\u043E\u0440\u043A\u0430\
  \ \u043A\u043E\u043D\u043A\u0440\u0435\u0442\u043D\u044B\u0445 \u0447\u0430\u0441\
  \u0442\u0435\u0439 \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\
  \u044E\u0442 \u044D\u0442\u043E, \u0447\u0442\u043E\u0431\u044B \u0438\u0437\u043E\
  \u043B\u0438\u0440\u043E\u0432\u0430\u0442\u044C, \u043C\u0430\u043D\u0438\u043F\
  \u0443\u043B\u0438\u0440\u043E\u0432\u0430\u0442\u044C \u0438\u043B\u0438 \u0430\
  \u043D\u0430\u043B\u0438\u0437\u0438\u0440\u043E\u0432\u0430\u0442\u044C\u2026"
title: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\u0434\
  \u0441\u0442\u0440\u043E\u043A"
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
