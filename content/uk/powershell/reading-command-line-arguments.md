---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що і чому?
Читання аргументів командного рядка - це можливість розробників отримувати вхідні дані безпосередньо при запуску програми. Це корисно для універсальності і автоматизації коду.

## Як це зробити:
Ось базовий приклад, як це можна зробити в PowerShell:
```PowerShell
param (
  [Parameter(Mandatory=$true)]
  [string]$name
)

Write-Host "Привіт, $name!"
```

При запуску цього скрипта з аргументом ви бачите наступне:
```PowerShell
.\Hello.ps1 -name "Василь"
```
*Вивід:*
```PowerShell
Привіт, Василь!
```

## Глибше занурення
Читання аргументів командного рядка було широко застосовано ще з моменту створення перших мов програмування. PowerShell використовує параметри в стилі UNIX, що робить його досить гнучким для розробки. Однак, для більш складних потреб ви також можете використовувати функції .NET Framework, наприклад, `[Environment]::GetCommandLineArgs()`.

## Подивитися також
1. Процедурне програмування PowerShell: [Введення в PowerShell](https://docs.microsoft.com/uk-ua/powershell/scripting/overview?view=powershell-7.1)