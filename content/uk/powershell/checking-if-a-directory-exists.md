---
title:                "Перевірка наявності директорії"
html_title:           "Go: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Що і чому?

Перевірка наявності каталогу - це процес, який дозволяє програмістам визначити, чи існує вказаний каталог. Це важливо для уникнення помилок при спробі доступу до невідомих ресурсів.

## Як це зробити:

Для перевірки наявності каталогу в PowerShell, вам потрібно використовувати cmdlet `Test-Path`. Давайте подивимося, як це працює:

```PowerShell
$directoryPath = "C:\SomeDirectory"
if (Test-Path $directoryPath)
{
    Write-Host "Directory exists."
}
else
{
    Write-Host "Directory does not exist."
}
```
Виведенням буде лише "Directory exists." або "Directory does not exist." в залежності від того, чи існує вказаний каталог.

## Поглиблений огляд:

**Історичний контекст**: PowerShell був вперше введений в 2006 році як Windows PowerShell. З цього часу, `Test-Path` була ключовою командою для перевірки наявності файлів або каталогів.

**Альтернативи**: Хоча `Test-Path` є найбільш поширеним варіантом для цієї задачі в PowerShell, можна використовувати інші методи, такі як `[System.IO.Directory]::Exists($directoryPath)`.

**Деталі реалізації**: `Test-Path` працює шляхом використання .NET Framework для отримання інформації про шлях. Це може означати, що його ефективність може залежати від версії .NET Framework, встановленої на вашій системі.

## Дивіться також:

- [Офіційна документація PowerShell](https://docs.microsoft.com/uk-ua/powershell/)
- [Cmdlet `Test-Path` в документації PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path?view=powershell-7.1)
- [Клас `System.IO.Directory` в документації .NET Framework](https://docs.microsoft.com/uk-ua/dotnet/api/system.io.directory?view=net-5.0)
- [Перевірка наявності файлу чи теки в PowerShell](https://www.computerperformance.co.uk/powershell/test-path/)