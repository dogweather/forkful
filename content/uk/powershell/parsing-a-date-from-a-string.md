---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:37:50.820892-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що & навіщо?
Парсинг дати із рядка -- це процес витягнення дати у стандартизованому форматі із тексту. Програмісти роблять це для обробки та аналізу часових даних в автоматизованому режимі.

## Як це зробити:
```PowerShell
# Parsю ввести дату у форматі рядка
$dateString = "12 квітня 2023"

# Використовую [datetime]::ParseExact(), щоб перетворити рядок у об'єкт DateTime
$format = "d MMMM yyyy"
$culture = [System.Globalization.CultureInfo]::GetCultureInfo("uk-UA")
$dateTime = [datetime]::ParseExact($dateString, $format, $culture)

# Вивід результату
$dateTime
```
Виходить:
```
середа, 12 квітня 2023 р. 0:00:00
```

## Поглиблено:
Колись дати вводилися вручну та перевірялися на помилки. Сучасні технології дозволяють автоматизувати цей процес. Метод `ParseExact` дозволяє вказати конкретний формат дати і культурний контекст (наприклад, українську локаль `uk-UA`). Є альтернативи: `Parse()`, який намагається розпізнати формат автоматично, і `TryParse()`, який використовується для безпечного парсингу, коли є ризик помилок у форматі дати.

## Дивись також:
- [Документація по класах DateTime і CultureInfo на Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [Про CultureInfo на Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
- [Про форматування дат і часу на Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
