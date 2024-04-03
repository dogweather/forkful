---
date: 2024-01-20 17:37:37.926172-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:49.669874-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

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
