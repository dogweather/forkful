---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:13:37.872986-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Wczytanie bieżącej daty to chleb powszedni w programowaniu - używamy tego do logów, timestampów i wszelkich funkcji opartych na czasie.

## How to:
Pobranie dzisiejszej daty w C# jest proste jak bułka z masłem. Używamy klasy `DateTime`. Oto jak to zrobimy:

```csharp
using System;

namespace GetCurrentDateExample
{
    class Program
    {
        static void Main(string[] args)
        {
            DateTime currentDate = DateTime.Now;
            Console.WriteLine(currentDate.ToString("yyyy-MM-dd"));
        }
    }
}
```

Przykładowe wyjście:
```
2023-04-05
```

## Deep Dive
`DateTime` jest standardowym typem w .NET do obsługi dat i czasu. Debiutowała w pierwszej wersji frameworka, a od tego czasu jest stałym elementem ekosystemu .NET. Kluczowe metody to `Now` i `UtcNow`, jedna daje ci lokalną datę i czas, druga - uniwersalny czas koordynowany. Alternatywą dla `DateTime` jest `DateTimeOffset`, które zawiera informacje o strefie czasowej - to ważne, gdy aplikacja działa globalnie. Warto też wiedzieć o `TimeZoneInfo`, aby poprawnie zarządzać czasem w różnych strefach.

## See Also
- Dokumentacja Microsoft dla `DateTime`: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- Wskazówki dotyczące użycia stref czasowych: [docs.microsoft.com](https://docs.microsoft.com/pl-pl/dotnet/standard/datetime/choosing-between-datetime)
- `DateTimeOffset` dokumentacja: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-6.0)
