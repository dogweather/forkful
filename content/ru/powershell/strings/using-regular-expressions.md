---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:03.056781-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: Regex \u044F\u0432\u043B\u044F\u044E\u0442\u0441\u044F \u043D\
  \u0435\u043E\u0442\u044A\u0435\u043C\u043B\u0435\u043C\u043E\u0439 \u0447\u0430\u0441\
  \u0442\u044C\u044E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0440\u043E\
  \u0432\u0430\u043D\u0438\u044F \u0441 1950-\u0445 \u0433\u043E\u0434\u043E\u0432\
  . \u0425\u043E\u0442\u044F \u0432 PowerShell \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u044B \u0442\u0430\u043A\u0438\u0435 cmdlet, \u043A\u0430\u043A `-match`,\
  \ `-replace` \u0438\u2026"
lastmod: '2024-04-05T22:50:58.822861-06:00'
model: gpt-4-0125-preview
summary: "Regex \u044F\u0432\u043B\u044F\u044E\u0442\u0441\u044F \u043D\u0435\u043E\
  \u0442\u044A\u0435\u043C\u043B\u0435\u043C\u043E\u0439 \u0447\u0430\u0441\u0442\u044C\
  \u044E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0440\u043E\u0432\u0430\
  \u043D\u0438\u044F \u0441 1950-\u0445 \u0433\u043E\u0434\u043E\u0432."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

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
