---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:37.743351-07:00
description: "Het berekenen van een toekomstige of verleden datum houdt in dat je\
  \ uitzoekt wat de datum was, of zal zijn, na of voor een bepaald tijdsinterval.\u2026"
lastmod: 2024-02-19 22:05:09.883796
model: gpt-4-0125-preview
summary: "Het berekenen van een toekomstige of verleden datum houdt in dat je uitzoekt\
  \ wat de datum was, of zal zijn, na of voor een bepaald tijdsinterval.\u2026"
title: Een datum in de toekomst of het verleden berekenen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het berekenen van een toekomstige of verleden datum houdt in dat je uitzoekt wat de datum was, of zal zijn, na of voor een bepaald tijdsinterval. Programmeurs hebben dit vaak nodig om gebeurtenissen te plannen, vervaldatums te hanteren, of tijdgevoelige gegevens te registreren.

## Hoe:

Toekomstige datums berekenen:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan oneWeek = TimeSpan.FromDays(7);
        
        DateTime nextWeek = currentDate + oneWeek;
        Console.WriteLine($"Over een week: {nextWeek}");
    }
}
```

Uitvoer:

```
Over een week: <datum een week vanaf de huidige datum>
```

Verleden datums berekenen:

```C#
using System;

class DateExample
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        TimeSpan tenDaysAgo = TimeSpan.FromDays(-10);
        
        DateTime pastDate = currentDate + tenDaysAgo;
        Console.WriteLine($"Tien dagen geleden was het: {pastDate}");
    }
}
```

Uitvoer:

```
Tien dagen geleden was het: <datum tien dagen voor de huidige datum>
```

## Diepgaande duik

In C#, zijn `DateTime` en `TimeSpan` de basis voor datum- en tijdoperaties. `DateTime` vertegenwoordigt een moment in tijd, doorgaans uitgedrukt als een datum en tijdstip van de dag. `TimeSpan` vertegenwoordigt een tijdsinterval.

Historisch gezien waren datum- en tijdberekeningen gevoelig voor fouten door de handmatige afhandeling van dagen, maanden en schrikkeljaren. `DateTime` abstraheert deze complexiteiten, waardoor het framework de lastige kwesties afhandelt.

Alternatieven voor `DateTime` en `TimeSpan` in .NET omvatten `DateTimeOffset`, die een tijdzoneverschuiving bevat, wat beter is voor toepassingen die over tijdzones heen werken. Een andere alternatief is Noda Time, een bibliotheek van Jon Skeet ontworpen voor meer complexe datum- en tijdbewerkingen, zoals verschillende kalenders.

Wat betreft implementatie, wanneer je een `TimeSpan` aan een `DateTime` toevoegt, manipuleert het onder de kap de ticks, de fundamentele tijdseenheid in .NET (`1 tick = 100 nanoseconden`). Voor verleden datums doet een negatieve `TimeSpan` het werk.

## Zie Ook

- .NET API-documentatie voor [`DateTime`](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime)
- Introductie tot [`TimeSpan`](https://docs.microsoft.com/nl-nl/dotnet/api/system.timespan)
- Microsoft's beste praktijken voor [`DateTime` en `DateTimeOffset`](https://docs.microsoft.com/nl-nl/dotnet/standard/datetime/choosing-between-datetime)
- Noda Time-documentatie: [https://nodatime.org](https://nodatime.org)
