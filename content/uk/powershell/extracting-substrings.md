---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і чому?

Видобування підрядків - це процес вибірки менших частин (рядків) з більшого рядка. Розробники використовують це для того, щоб обробляти менші фрагменти даних, що полегшують розбір інформації.

## Як це робити:

В PowerShell ви можете використовувати метод `.Substring()`, щоб видобути підрядок. Ось як це робиться:
```PowerShell
$string = "Привіт, Світ!"
$substring = $string.Substring(0,6)
Write-Output $substring
```
Вихід:
```PowerShell
Привіт
```
У цьому прикладі, ми бачимо, що `.Substring(0,6)` видобуває перші 6 символів з рядка.

## Поглиблено

### Історичний контекст
Метод `.Substring()` існує вже давно в багатьох мовах програмування, включаючи C#, Java і Python.

### Альтернативи
Інша можливість - використання квадратних дужок для видобування підрядка:
```PowerShell
$string = "Привіт, Світ!"
$substring = $string[0..5] -join ""
Write-Output $substring
```

### Деталі реалізації
Є два аргументи, які йдуть з .Substring(): `startIndex` і `length`. `startIndex` - це позиція, з якої починається видобування підрядка, а `length` - кількість символів, які необхідно видобувти.

## Дивись також
- [MSDN Documentation for .Substring() method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netcore-3.1)
- [Guide to Getting Substrings in PowerShell at adamtheautomator.com](https://adamtheautomator.com/powershell-substring/)