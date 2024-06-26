---
date: 2024-01-20 17:31:55.983163-07:00
description: "Jak to zrobi\u0107: W przesz\u0142o\u015Bci do obliczania dat u\u017C\
  ywano r\xF3\u017Cnych system\xF3w i narz\u0119dzi \u2013 od kalendarzy papierowych\
  \ po proste programy komputerowe. W\u2026"
lastmod: '2024-04-05T21:53:37.068514-06:00'
model: gpt-4-1106-preview
summary: "W przesz\u0142o\u015Bci do obliczania dat u\u017Cywano r\xF3\u017Cnych system\xF3\
  w i narz\u0119dzi \u2013 od kalendarzy papierowych po proste programy komputerowe."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

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
