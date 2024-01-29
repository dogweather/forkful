---
title:                "Een datum uit een string parsen"
date:                  2024-01-28T22:03:55.508451-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een datum uit een string parsen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een datum uit een string parsen betekent het converteren van tekst die een datum vertegenwoordigt naar een `DateTime` object. Dit is cruciaal voor het opslaan en interpreteren van datums uit verschillende formaten als daadwerkelijke datums in je code.

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
