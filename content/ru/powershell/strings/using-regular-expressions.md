---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:03.056781-07:00
description: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0435 \u0432\u044B\
  \u0440\u0430\u0436\u0435\u043D\u0438\u044F (regex) \u2014 \u044D\u0442\u043E \u043C\
  \u043E\u0449\u043D\u044B\u0435 \u043F\u0430\u0442\u0442\u0435\u0440\u043D\u044B\
  \ \u0434\u043B\u044F \u0441\u043E\u043F\u043E\u0441\u0442\u0430\u0432\u043B\u0435\
  \u043D\u0438\u044F \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0445 \u0441\
  \u0442\u0440\u043E\u043A. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ regex \u0434\u043B\u044F \u043F\u043E\u0438\u0441\u043A\u0430, \u043F\u0440\u043E\
  \u0432\u0435\u0440\u043A\u0438 \u0438\u043B\u0438 \u0437\u0430\u043C\u0435\u043D\
  \u044B\u2026"
lastmod: '2024-03-13T22:44:45.428135-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0435 \u0432\u044B\
  \u0440\u0430\u0436\u0435\u043D\u0438\u044F (regex) \u2014 \u044D\u0442\u043E \u043C\
  \u043E\u0449\u043D\u044B\u0435 \u043F\u0430\u0442\u0442\u0435\u0440\u043D\u044B\
  \ \u0434\u043B\u044F \u0441\u043E\u043F\u043E\u0441\u0442\u0430\u0432\u043B\u0435\
  \u043D\u0438\u044F \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0445 \u0441\
  \u0442\u0440\u043E\u043A. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ regex \u0434\u043B\u044F \u043F\u043E\u0438\u0441\u043A\u0430, \u043F\u0440\u043E\
  \u0432\u0435\u0440\u043A\u0438 \u0438\u043B\u0438 \u0437\u0430\u043C\u0435\u043D\
  \u044B\u2026"
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

## Что и Почему?
Регулярные выражения (regex) — это мощные паттерны для сопоставления текстовых строк. Программисты используют regex для поиска, проверки или замены содержимого эффективно.

## Как использовать:
```PowerShell
# Найти соответствие паттерну, начинающемуся с 'S', за которым следуют любые символы, завершающиеся на 'e'
$pattern = 'S.*e'
$text = 'Sample sentence in PowerShell.'
if ($text -match $pattern) {
    "Найдено соответствие: $($matches[0])"
}

# Заменить все вхождения 'dog' на 'cat'
$petStory = 'The quick brown dog jumps over the lazy dog.'
$petStory -replace 'dog', 'cat'
```
Вывод:
```
Найдено соответствие: Sample sentence in
The quick brown cat jumps over the lazy cat.
```

## Подробнее
Regex являются неотъемлемой частью программирования с 1950-х годов. Хотя в PowerShell встроены такие cmdlet, как `-match`, `-replace` и `Select-String` для работы с regex, существуют альтернативы для манипуляции с текстом – например `string.Contains` или `string.Replace`. Regex в PowerShell использует реализацию .NET framework, благодаря чему он надежен и богат функциональностью.

## Смотрите также
- [Официальная справка по regex от Microsoft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- [Regular-Expressions.info](https://www.regular-expressions.info/powershell.html)
- [Regex101: Создание и тестирование regex](https://regex101.com/)
