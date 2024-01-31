---
title:                "Перетворення дати в рядок"
date:                  2024-01-20T17:37:37.926172-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?
Конвертація дати у рядок – це процес перетворення об’єкта дати в текстовий формат. Робимо це для відображення дат у зручному для читання форматі або для їхнього збереження та обміну даними.

## Як це зробити:
```PowerShell
# Отримання поточної дати
$currentDate = Get-Date

# Форматування дати у рядок
$dateString = $currentDate.ToString("yyyy-MM-dd")
Write-Host "Форматована дата: $dateString"

# Вивід
# Форматована дата: 2023-04-05
```

## Поглиблено:
Конвертування дати в рядок у PowerShell було запроваджене разом із самою мовою і є важливим для скриптів, що взаємодіють із файлами та іншими системами. Альтернативи включають використання стандартних методів .NET класу `DateTime`, таких як `ToShortDateString()` або `ToLongDateString()`, але `ToString("format")` дає більше контролю. Детальніше, PowerShell використовує клас `DateTime` з .NET Framework для роботи із датами, який дозволяє легко конвертувати їх у рядки, використовуючи задані формати.

## Дивіться також:
- [Про DateTime на сайті Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
