---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:55.508451-07:00
description: "Hoe te: Voor `DateTime` vertrouwden programmeurs op eigen code om datums\
  \ te verwerken, wat gevoelig was voor fouten en ineffici\xEBnties. De `DateTime`\u2026"
lastmod: '2024-04-05T21:53:50.843612-06:00'
model: gpt-4-0125-preview
summary: "Voor `DateTime` vertrouwden programmeurs op eigen code om datums te verwerken,\
  \ wat gevoelig was voor fouten en ineffici\xEBnties."
title: Een datum uit een string parsen
weight: 30
---

## Hoe te:
```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "2023-03-15";
        DateTime parsedDate = DateTime.Parse(dateString);
        Console.WriteLine(parsedDate); // Output: 15-3-2023 00:00:00
        
        // Met specifiek formaat
        dateString = "15 maart, 2023";
        string format = "d MMMM, yyyy";
        CultureInfo provider = CultureInfo.InvariantCulture;
        parsedDate = DateTime.ParseExact(dateString, format, provider);
        Console.WriteLine(parsedDate); // Output: 15-3-2023 00:00:00
    }
}
```

## Diepere duik
Voor `DateTime` vertrouwden programmeurs op eigen code om datums te verwerken, wat gevoelig was voor fouten en inefficiënties. De `DateTime` struct in .NET heeft dit gerevolutioneerd door robuuste parseermethoden te bieden - `Parse` en `ParseExact`.

`Parse` probeert een datumstring te begrijpen op basis van cultuurspecifieke of universele formaten. Prima wanneer je standaard datumformaten verwacht. Echter, als je te maken hebt met specifieke of ongebruikelijke datumformaten, is `ParseExact` (samen met `TryParse` en `TryParseExact` voor foutafhandeling) je vriend. Hier dicteer je het exacte formaat met een aangepast patroon.

De implementatie gebruikt de `CultureInfo` klasse om respect te tonen voor verschillende culturele datumformaten. Door `ParseExact` te gebruiken, vermijd je culturele misverstanden - jouw gedefinieerde patroon is wat telt. Onthoud dat computerverdatums beginnen vanaf 1 januari 0001, dus zorg ervoor dat je string een geldige datum vertegenwoordigt binnen het bereik van de .NET kalender.

## Zie ook
- [Documentatie van de DateTime.Parse Methode](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime.parse)
- [Documentatie van de DateTime.ParseExact Methode](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime.parseexact)
- [Aangepaste datum- en tijdsopmaakstrings](https://docs.microsoft.com/nl-nl/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Klasse](https://docs.microsoft.com/nl-nl/dotnet/api/system.globalization.cultureinfo)
