---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:41.381930-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\u0430\u043F\u0438\
  \u0448\u0435\u043C \u0444\u0443\u043D\u043A\u0446\u0438\u044E \u0434\u043B\u044F\
  \ \u0440\u0430\u0441\u0447\u0435\u0442\u0430 \u0441\u0443\u043C\u043C\u044B \u0434\
  \u0432\u0443\u0445 \u0447\u0438\u0441\u0435\u043B. \u041F\u0440\u043E\u0441\u0442\
  \u043E, \u043D\u043E \u044D\u0442\u043E \u043D\u0430\u0433\u043B\u044F\u0434\u043D\
  \u043E \u0438\u043B\u043B\u044E\u0441\u0442\u0440\u0438\u0440\u0443\u0435\u0442\
  \ \u0441\u0443\u0442\u044C."
lastmod: '2024-03-13T22:44:45.461926-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043D\u0430\u043F\u0438\u0448\
  \u0435\u043C \u0444\u0443\u043D\u043A\u0446\u0438\u044E \u0434\u043B\u044F \u0440\
  \u0430\u0441\u0447\u0435\u0442\u0430 \u0441\u0443\u043C\u043C\u044B \u0434\u0432\
  \u0443\u0445 \u0447\u0438\u0441\u0435\u043B."
title: "\u041E\u0440\u0433\u0430\u043D\u0438\u0437\u0430\u0446\u0438\u044F \u043A\u043E\
  \u0434\u0430 \u0432 \u0444\u0443\u043D\u043A\u0446\u0438\u0438"
weight: 18
---

## Как это сделать:
Давайте напишем функцию для расчета суммы двух чисел. Просто, но это наглядно иллюстрирует суть.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Вызов функции с 5 и 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "Сумма равна $sum"
```

Пример вывода:

```
Сумма равна 15
```

## Глубокое погружение
Функции в PowerShell, как и в большинстве языков программирования, не новость. Мы компартментализируем код с дней Fortran. Речь идет о том, чтобы "не изобретать велосипед". Альтернативы? Конечно, скрипты или cmdlet. Но им не хватает аккуратности и контекстно-зависимой организации, как у функций внутри скриптов.

Реализация? Функции могут быть простыми, как наш пример, или сложными, с областями видимости, входными данными из конвейера и многим другим. Возьмем `Расширенные Функции`. Они имитируют cmdlet с параметрами, имеющими атрибуты, например, `[Parameter(Mandatory=$true)]`. Это лишь малая часть гибкости PowerShell.

## Смотрите также
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
