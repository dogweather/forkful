---
date: 2024-01-20 17:33:37.125073-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: PowerShell\
  \ uses the `System.DateTime` object for date operations \u2013 it's standard since\
  \ .NET. Before, we juggled timestamp conversions.\u2026"
lastmod: '2024-04-05T21:53:49.803573-06:00'
model: gpt-4-1106-preview
summary: "PowerShell uses the `System.DateTime` object for date operations \u2013\
  \ it's standard since .NET."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Як це робити:
```PowerShell
# Визначаємо дві дати
$date1 = Get-Date '2023-03-01'
$date2 = Get-Date '2023-03-15'

# Порівнюємо дати
if ($date1 -lt $date2) {
    Write-Output "Дата1 раніше Дати2"
} elseif ($date1 -gt $date2) {
    Write-Output "Дата1 пізніше Дати2"
} else {
    Write-Output "Дата1 і Дата2 однакові"
}
```

## Поглиблене вивчення:
PowerShell uses the `System.DateTime` object for date operations – it's standard since .NET. Before, we juggled timestamp conversions. Alternatives in other languages include `DateTime` in C#, `date` in PHP, or libraries like Joda-Time in Java.

In PowerShell, dates are compared using `-lt` (less than), `-gt` (greater than), and `-eq` (equal to). Under the hood, it compares ticks (the number of 100-nanosecond intervals since 1 January 0001).

Earlier versions of scripting tools lacked robust date comparison features, making tasks like calculating age or finding overdue items trickier. PowerShell streamlined this with its object-oriented approach.

## Дивіться також:
- [Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
