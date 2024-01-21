---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:31:55.983163-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Obliczanie daty w przyszłości lub przeszłości to sposób na ustalenie daty, która jest określoną liczbę dni, miesięcy, czy lat od daty wyjściowej. Programiści robią to, aby zarządzać terminami, wydarzeniami, czy też okresami ważności w aplikacjach.

## Jak to zrobić:
```PowerShell
# Obliczanie daty 10 dni w przyszłości
$dzis = Get-Date
$przyszlosc = $dzis.AddDays(10)
"Data za 10 dni to: $przyszlosc"

# Obliczanie daty 5 dni w przeszłości
$przeszlosc = $dzis.AddDays(-5)
"Data sprzed 5 dni to: $przeszlosc"

# Przykładowe wyjście
Data za 10 dni to: czwartek, 10 lutego 2023 23:05:49
Data sprzed 5 dni to: sobota, 26 stycznia 2023 23:05:49
```

## Deep Dive
W przeszłości do obliczania dat używano różnych systemów i narzędzi – od kalendarzy papierowych po proste programy komputerowe. W PowerShellu obliczanie dat jest proste dzięki wbudowanym funkcjom, jak np. `AddDays()`, ale można też użyć `AddHours()`, `AddMonths()` i inne. Alternatywą jest także manualne dodawanie sekund, minut czy godzin do timestampów. Ważne jest zrozumienie stref czasowych i lokalnych ustawień czasu, by operacje na datach były poprawne w różnych środowiskach.

## Zobacz także
- [Dokumentacja PowerShell dla DateTime](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [O strefach czasowych w .NET](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)