---
title:                "C#: Beregning av en dato i fremtiden eller fortiden"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor
Det kan være nyttig å kunne beregne datoer i fremtiden eller i fortiden for å kunne planlegge hendelser eller for å forstå tidligere hendelser.

# Slik gjør du det
```C#
// Beregne dato i fremtiden
DateTime today = DateTime.Today;
DateTime futureDate = today.AddDays(7); // Legg til 7 dager for å få dato i neste uke

Console.WriteLine("Dato i neste uke: " + futureDate.ToShortDateString()); // Skriver ut datoen på kort format, f.eks 02.06.2020
```

```C#
// Beregne dato i fortiden
DateTime today = DateTime.Today;
DateTime pastDate = today.AddDays(-30); // Trekk fra 30 dager for å få dato for en måned tilbake

Console.WriteLine("Dato for en måned siden: " + pastDate.ToShortDateString()); // Skriver ut datoen på kort format, f.eks 02.05.2020
```

# Dypdykk
Å beregne datoer i fremtiden eller i fortiden innebærer å forstå hvordan datoer håndteres i programmet og hvordan matematiske operasjoner kan brukes for å endre datoverdien. Man må også være oppmerksom på ulike datoformater og sørge for at utskrift av datoene blir gjort på ønsket format.

# Se også
- [DateTime Struktur (Microsoft Dokumentasjon)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- [How to: Add Days to a Date (C# Programmeringsguide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/numbers-dates/how-to-add-interval-to-a-date)
- [Custom Date and Time Format Strings (Microsoft Dokumentasjon)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)