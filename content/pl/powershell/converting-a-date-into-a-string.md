---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "PowerShell: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwertowanie daty na ciąg znaków to proces zmiany daty w formacie tekstowym, np. 2021-10-01, na ciąg znaków, który może być wykorzystany w innych funkcjach lub operacjach programistycznych. Programiści wykonują to, gdy potrzebują manipulować lub porównywać daty.

## Jak to zrobić:
```PowerShell
# Przykład 1: Konwertowanie daty na ciąg znaków w formacie rrrr-mm-dd
Get-Date -Format 'yyyy-MM-dd'
// Wynik: 2021-10-01

# Przykład 2: Konwertowanie daty na niezmienioną ciąg znaków
Get-Date -Format FileDateTime
// Wynik: 20211001105856

# Przykład 3: Konwertowanie daty na słowny opis
$today = Get-Date
$today.ToString('dddd, dd MMMM yyyy')
// Wynik: piątek, 01 października 2021
```

## Głębsze zagadnienia:
Konwersja daty na ciąg znaków jest niezbędnym elementem programowania, ponieważ daty są jednym z najczęściej używanych typów danych. Istnieje wiele narzędzi lub funkcji, które mogą wykonać tę operację, takich jak `Convert-TimeToUtc` lub `ToString`. W dalszej przeszłości, konwersja dat może być również potrzebna do wykonywania zaawansowanych operacji, takich jak obliczanie różnicy między dwoma danymi, rozpoznawanie dat w różnych językach lub wyświetlanie dat w innych formatach.

## Zobacz także:
- [Dokumentacja Microsoft PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/)
- [Poradniki i przykłady PowerShell](https://github.com/PowerShell/PowerShell-examples)
- [Forum społeczności PowerShell](https://community.idera.com/database-tools/powershell/)