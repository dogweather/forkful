---
date: 2024-01-20 17:51:21.953687-07:00
description: Interpolating a string is inserting values into a string template. Programmers
  do it to dynamically build strings and make code more readable.
lastmod: '2024-03-13T22:44:49.625368-06:00'
model: gpt-4-1106-preview
summary: Interpolating a string is inserting values into a string template.
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 8
---

## How to: (Як це зробити:)
```PowerShell
$name = "Влад"
$greeting = "Привіт, $name! Як твій день?"

# Output: Зверніть увагу, що значення змінної $name підставлене в рядок.
Write-Host $greeting 
```

## Deep Dive (Поглиблений Аналіз)
String interpolation allows for embedded variable expressions. It's been around since Windows PowerShell 4.0 and is basic yet powerful. Alternatives like `-f` format operator exist but lack simplicity. Interpolation is done at runtime, parsing the string and replacing variables with values.

## See Also (Дивіться також)
- [About_Quoting_Rules](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_quoting_rules)
- [Powershell's -f Format Operator](https://ss64.com/ps/syntax-f-operator.html)
