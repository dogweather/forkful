---
date: 2024-01-26 03:43:24.859800-07:00
description: "\xC5 avrunde tall betyr \xE5 justere dem til den n\xE6rmeste spesifiserte\
  \ plassverdien\u2014tenk \xE5 spenne dem ned til en enklere form. Programmerere\
  \ runder av for \xE5\u2026"
lastmod: '2024-02-25T18:49:38.966119-07:00'
model: gpt-4-0125-preview
summary: "\xC5 avrunde tall betyr \xE5 justere dem til den n\xE6rmeste spesifiserte\
  \ plassverdien\u2014tenk \xE5 spenne dem ned til en enklere form. Programmerere\
  \ runder av for \xE5\u2026"
title: Avrunding av tall
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å avrunde tall betyr å justere dem til den nærmeste spesifiserte plassverdien—tenk å spenne dem ned til en enklere form. Programmerere runder av for å kontrollere presisjon, øke ytelse eller når de viser brukervennlige resultater—som priser som ikke trenger tre desimalplasser.

## Hvordan:
Her er billetten for å avrunde tall i C#:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // Avrunde til nærmeste hele tall
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // Utdata: 123

        // Angi antall desimalplasser
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // Utdata: 123.46

        // Avrund opp uansett neste siffer
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // Utdata: 124

        // Avrund ned uansett neste siffer
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // Utdata: 123
    }
}
```

## Dypdykk
I gamle dager var avrunding en enkel måte å trimme beregningskostnader på. Hver syklus talte, og å trimme tall sparte dyrebar tid. Hurtigsprang til moderne C#, og det handler om å håndtere dobbelt- og desimaltallenes beryktede forutsetning til presisjonsfeil og visningsquirks.

Utover `Math.Round`, `Math.Floor`, og `Math.Ceiling`, lar `MidpointRounding`-enumerasjonen oss diktere skjebnen til stakkars midtsittende sifre—det er et veiskille mellom bankregler og lekeplassrettferdigheten av "avrunde halv opp".

For tøffere publikum, som seriøs matematikk eller finansapplikasjoner, har vi `decimal` over `double`, som reduserer på avrundingsdrama ved å tilby høyere presisjon—mindre avrunding, færre problemer.

## Se også
- [Offisielle C# Dokumenter om `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Når bør jeg bruke Double i stedet for Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [IEEE Standard for flyttallaritmetikk (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
