---
date: 2024-01-26 01:16:31.855166-07:00
description: "\u042F\u043A: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\u0430\
  \u043F\u0438\u0448\u0435\u043C\u043E \u0444\u0443\u043D\u043A\u0446\u0456\u044E\
  \ \u0434\u043B\u044F \u0440\u043E\u0437\u0440\u0430\u0445\u0443\u043D\u043A\u0443\
  \ \u0441\u0443\u043C\u0438 \u0434\u0432\u043E\u0445 \u0447\u0438\u0441\u0435\u043B\
  . \u041F\u0440\u043E\u0441\u0442\u043E, \u0430\u043B\u0435 \u0446\u0435 \u0456\u043B\
  \u044E\u0441\u0442\u0440\u0443\u0454 \u0456\u0434\u0435\u044E."
lastmod: '2024-03-13T22:44:49.659342-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\u0430\u043F\u0438\u0448\
  \u0435\u043C\u043E \u0444\u0443\u043D\u043A\u0446\u0456\u044E \u0434\u043B\u044F\
  \ \u0440\u043E\u0437\u0440\u0430\u0445\u0443\u043D\u043A\u0443 \u0441\u0443\u043C\
  \u0438 \u0434\u0432\u043E\u0445 \u0447\u0438\u0441\u0435\u043B."
title: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\u043E\
  \u0434\u0443 \u0432 \u0444\u0443\u043D\u043A\u0446\u0456\u0457"
weight: 18
---

## Як:
Давайте напишемо функцію для розрахунку суми двох чисел. Просто, але це ілюструє ідею.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Викликати функцію з 5 та 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "Сума становить $sum"
```

Приклад виводу:

```
Сума становить 15
```

## Поглиблений огляд
Функції в PowerShell, як і в більшості мов, не новина. Ми організовуємо код на окремі блоки ще з часів Fortran. Йдеться про "неперевинаходження колеса". Альтернативи? Звичайно, скрипти чи командлети. Але вони не мають такої акуратності та контекстної чутливості, як функції всередині скриптів.

Реалізація? Функції можуть бути простими, як наш приклад, або складними з областями видимості, введенням через конвеєр і більше. Візьміть `Розширені Функції`. Вони імітують командлети з параметрами, які мають атрибути, наприклад `[Parameter(Mandatory=$true)]`. Ось невеликий приклад гнучкості PowerShell.

## Дивіться також
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
