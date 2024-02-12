---
title:                "Использование регулярных выражений"
aliases:
- /ru/powershell/using-regular-expressions.md
date:                  2024-01-29T00:04:03.056781-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
