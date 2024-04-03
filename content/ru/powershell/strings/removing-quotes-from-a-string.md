---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:36.075583-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u043E\u043F\u0435\u0440\u0430\
  \u0442\u043E\u0440 `-replace` \u0434\u043B\u044F \u0443\u0434\u0430\u043B\u0435\u043D\
  \u0438\u044F \u043A\u0430\u0432\u044B\u0447\u0435\u043A \u0438\u0437 \u0441\u0442\
  \u0440\u043E\u043A\u0438. \u0412\u043E\u0442 \u043A\u0430\u043A."
lastmod: '2024-03-13T22:44:45.424580-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u043E\u043F\u0435\u0440\u0430\u0442\
  \u043E\u0440 `-replace` \u0434\u043B\u044F \u0443\u0434\u0430\u043B\u0435\u043D\u0438\
  \u044F \u043A\u0430\u0432\u044B\u0447\u0435\u043A \u0438\u0437 \u0441\u0442\u0440\
  \u043E\u043A\u0438."
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\u0447\
  \u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 9
---

## Как это сделать:
Вы можете использовать оператор `-replace` для удаления кавычек из строки. Вот как:

```PowerShell
# Замена одинарных кавычек
$stringWithSingleQuotes = "'Привет, мир!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # Вывод: Привет, мир!

# Замена двойных кавычек
$stringWithDoubleQuotes = '"Привет, мир!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # Вывод: Привет, мир!
```

Для обоих типов:

```PowerShell
$stringWithQuotes = '"Привет," сказала она.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # Обратите внимание на использование класса символов regex
Write-Output $cleanString  # Вывод: Привет, сказала она.
```

Пример вывода в консоли будет выглядеть примерно так:

```
Привет, мир!
Привет, мир!
Привет, сказала она.
```

## Глубже
В старые времена, до того как PowerShell появился на свет в Microsoft, обработка текста в Windows часто была делом пакетных сценариев, имеющих ограниченные возможности. Появление PowerShell принесло с собой мощные функции манипуляции с текстом, которые сделали написание скриптов гораздо более мощным.

Существуют альтернативы `-replace`, такие как использование метода `.Trim()` для удаления кавычек только в начале и в конце строки, но они не предлагают такого же контроля или поддержки regex.

```PowerShell
# Использование .Trim() для кавычек в начале и в конце
$stringWithQuotes = '"Привет, мир!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # Вывод: Привет, мир!
```

Обратите внимание, что `-replace` использует regex «под капотом», так что, когда вы работаете с ним, помните, что специальные символы нужно экранировать, если они являются вашей целью. Если вам нужен более детальный контроль над удалением кавычек, глубокое изучение regex с `-replace` - это то, что вам нужно, давая вам огромную гибкость.

## См. также
- Для получения дополнительной информации о regex в PowerShell, смотрите официальную документацию: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- Откройте для себя другие методы работы со строками: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
