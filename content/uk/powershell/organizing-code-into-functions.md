---
title:                "Організація коду в функції"
aliases:
- uk/powershell/organizing-code-into-functions.md
date:                  2024-01-26T01:16:31.855166-07:00
model:                 gpt-4-0125-preview
simple_title:         "Організація коду в функції"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Що і чому?
Організація коду у функціях полягає в обгортанні блоків коду, що виконують певні завдання, та наданні їм назви. Це робиться для того, щоб код був повторно використовуваним, зрозумілим та легким для підтримки. Замість повторного написання одного й того ж коду, викликайте функцію. Хочете здійснити пошук несправностей або оновлення? Внесіть зміни до функції без необхідності розгрібати купи скриптів.

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
