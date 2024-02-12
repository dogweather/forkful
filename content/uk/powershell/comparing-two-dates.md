---
title:                "Порівняння двох дат"
aliases:
- uk/powershell/comparing-two-dates.md
date:                  2024-01-20T17:33:37.125073-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?
Comparing two dates means checking which one comes first or if they're the same. Programmers do this to manage events, sort records, or check durations.

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
