---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:56:48.588392-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Програми отримують дані через аргументи командного рядка для більшої гнучкості. Програмісти використовують це, щоб налаштувати поведінку своїх скриптів без зміни коду.

## How to: (Як це зробити:)
Отримати доступ до аргументів командного рядка в PowerShell просто. Використовуйте `$args`, змінну, яка містить масив аргументів.

```PowerShell
# Цей скрипт виведе всі передані йому аргументи
param(
    [String]$name,
    [Int]$age 
)

Write-Host "Name: $name"
Write-Host "Age: $age"
Write-Host "Other Arguments:"

$args | ForEach-Object {
    Write-Host "Argument: $_"
}

# Спробуйте це у командному рядку:
# powershell -file .\yourscript.ps1 -name "Viktor" -age 30 Extra1 Extra2
```

Sample output:

```
Name: Viktor
Age: 30
Other Arguments:
Argument: Extra1
Argument: Extra2
```

## Deep Dive (Поглиблений Аналіз)
У минулих версіях Windows PowerShell, `$args` була основним способом доступу до аргументів. Однак, більш структурованим є використання параметрів (`param()` блоку) з типізацією. Альтернативи `Get-Opt` на PowerShell не так поширені, як у інших оболонках. Щодо впровадження, параметри командного рядка передаються у масиві та індексуються від нуля. Пам'ятайте про "quoting" при роботі з аргументами, що містять пробіли чи спеціальні символи.

## See Also (Дивіться Також):
- [Блог PowerShell з прикладами скриптів](https://devblogs.microsoft.com/powershell/)
