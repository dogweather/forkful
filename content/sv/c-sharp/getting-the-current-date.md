---
title:                "Att få den aktuella datumet"
html_title:           "C#: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den aktuella datumen innebär att hämta det nuvarande datumet och tiden på en enhets klocka. Programvaruprogrammerare gör detta för att fånga tidsinformation för användning i applikationer och algoritmer eller för att visa den aktuella tiden i användargränssnittet.

## Hur man gör det:
```C#
DateTime nu = DateTime.Now;
Console.WriteLine(nu);
```
Output: 25/11/2021 16:30:00

## Djupdykning:
Att få den aktuella datumen är ett grundläggande koncept i programmering och har funnits sedan tidiga datorer. Det finns flera alternativ för att få den aktuella datumen, såsom att hämta det via en nätverkstjänst eller beräkna det baserat på systemets räknare. I C# används vanligtvis DateTime-klassen för att hantera datum och tid. Det finns också olika format för att visa datumet, som kan ställas in med hjälp av ToString() metoden.

## Se även:
För mer information om hur man använder DateTime-klassen i C#, se [Microsofts dokumentation] (https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0) eller [date and time information] (https://docs.microsoft.com/en-us/dotnet/standard/base-types/date-time-blog).