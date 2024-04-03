---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:30.294349-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Elm \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0444\
  \u0443\u043D\u043A\u0446\u0438\u044E `String.toLower` \u0434\u043B\u044F \u043F\u0440\
  \u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\u044F \u0442\u0435\
  \u043A\u0441\u0442\u0430."
lastmod: '2024-03-13T22:44:44.877182-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0444\u0443\
  \u043D\u043A\u0446\u0438\u044E `String.toLower` \u0434\u043B\u044F \u043F\u0440\u0435\
  \u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\u044F \u0442\u0435\u043A\
  \u0441\u0442\u0430."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0438\
  \u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 4
---

## Как это сделать:
Elm использует функцию `String.toLower` для преобразования текста:

```elm
import String

lowercaseString : String -> String
lowercaseString text =
    String.toLower text

-- Использование
result : String
result =
    lowercaseString "HeLLo, WoRLD!"

-- Вывод: "hello, world!"
```

## Подробнее
`String.toLower` в Elm поступает из основной библиотеки `String` Elm, учитывая интернационализацию. Исторически преобразование регистра эволюционировало от базового ASCII до полной поддержки Unicode из-за необходимости обработки международных текстов.

В некоторых языках, таких как Javascript, существуют альтернативы, такие как `toLowerCase()` и `toLocaleLowerCase()`, где последняя учитывает специфические для локали правила. В Elm `String.toLower` должно быть достаточно для большинства случаев, если не имеются в виду операции, чувствительные к локали, которые могут потребовать пользовательскую реализацию.

Важной деталью является то, что преобразование регистра не всегда является один-в-один; некоторые символы могут не иметь эквивалента в нижнем регистре, а другие могут изменить размер (например, преобразование "ß" на немецком).

## Смотрите также
- Документация по строкам в Elm: [https://package.elm-lang.org/packages/elm/core/latest/String#toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Преобразование регистра в Unicode: [https://www.w3.org/International/wiki/Case_folding](https://www.w3.org/International/wiki/Case_folding)
- Проблемы преобразования регистра специфичные для языка: [https://stackoverflow.com/questions/234591/upper-vs-lower-case](https://stackoverflow.com/questions/234591/upper-vs-lower-case)
