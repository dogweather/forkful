---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:58:16.696850-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і чому?
Перевірка наявності каталогу - це спосіб дізнатись, чи існує папка на диску. Програмісти це роблять, щоб уникнути помилок при роботі з файлами та каталогами, які можуть не існувати.

## Як це зробити:
Перевірка з допомогою cmdlet `Test-Path`:

```PowerShell
# Перевіримо, чи є каталог 'C:\MyFolder'
$folderPath = 'C:\MyFolder'
$exists = Test-Path $folderPath
Write-Host "Каталог існує: $exists"
```

Якщо каталог існує, виведе `Каталог існує: True`; якщо ні – `Каталог існує: False`.

Створення каталогу, якщо він не існує:

```PowerShell
if (-not (Test-Path $folderPath)) {
    New-Item -ItemType Directory -Path $folderPath
    Write-Host "Створено каталог: $folderPath"
} else {
    Write-Host "Каталог вже існує: $folderPath"
}
```

## Поглиблено:
Перевірка існування каталогу стала частиною PowerShell з самого початку. `Test-Path` не тільки перевіряє каталоги, а й файли та інші об'єкти виведення. 

Альтернатива — це використання .NET класу `System.IO.Directory`. Однак `Test-Path` є "powershell-way" і краще інтегрується з іншими cmdlets.

Можливі параметри `Test-Path` включають `-PathType` для специфікації типу об'єкта (наприклад, `Container` для каталогів) та `-LiteralPath`, який використовується, коли шлях містить спеціальні символи.

## Дивись також:
- [About Test-Path](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
- [PowerShell 101](https://channel9.msdn.com/Series/GetStartedPowerShell3)