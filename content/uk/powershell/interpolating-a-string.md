---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:51:21.953687-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Interpolating a string is inserting values into a string template. Programmers do it to dynamically build strings and make code more readable.

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
