---
title:                "Организация кода в функции"
aliases:
- /ru/powershell/organizing-code-into-functions.md
date:                  2024-01-28T23:59:41.381930-07:00
model:                 gpt-4-0125-preview
simple_title:         "Организация кода в функции"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Организация кода в функции заключается в объединении блоков кода, выполняющих конкретные задачи, и присвоении им имени. Это делается для того, чтобы код можно было использовать повторно, чтобы он был читаемым и удобным для поддержки. Вместо того чтобы переписывать один и тот же код, вызывайте функцию. Хотите провести отладку или обновление? Измените функцию, не перебирая кучи скрипта.

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
