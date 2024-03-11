---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:17.236156-07:00
description: "De huidige datum in C# krijgen betekent het hier-en-nu van de klok van\
  \ je systeem plukken. Het is handig voor tijdstempels, logs, of elke functie die\
  \ een\u2026"
lastmod: '2024-03-11T00:14:24.647735-06:00'
model: gpt-4-0125-preview
summary: "De huidige datum in C# krijgen betekent het hier-en-nu van de klok van je\
  \ systeem plukken. Het is handig voor tijdstempels, logs, of elke functie die een\u2026"
title: Het huidige datum ophalen
---

{{< edit_this_page >}}

## Wat & Waarom?

De huidige datum in C# krijgen betekent het hier-en-nu van de klok van je systeem plukken. Het is handig voor tijdstempels, logs, of elke functie die een datumcontrole nodig heeft.

## Hoe te:

De huidige datum krijgen? Roep gewoon `DateTime.Now` aan. Dit stukje code laat zien hoe:

```C#
using System;

class GetCurrentDate
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate);
    }
}
```

Als je het uitvoert, verwacht dan zoiets als dit:

```
25-3-2023 11:34:52
```

Netjes, hè?

## Diepere Duik

Voor `DateTime` jongleerden programmeurs datum-tijd in hun hoofd. Nu, maakt .NET het gestroomlijnd. `DateTime.Now` pakt zowel datum als tijd, maar voor alleen de datum is er `DateTime.Today`.

Hier is een kicker – het houdt rekening met tijdzones. `DateTime.UtcNow` geeft je Coordinated Universal Time (UTC), waarbij je het drama van de lokale tijd vermijdt.

Historisch gezien was tijd bijhouden een rommeltje – denk aan zonnewijzers, waterklokken, noem maar op. Computers hebben het vereenvoudigd, maar tijdzones en regels voor zomertijd compliceren de zaken nog steeds. Gelukkig komt C# verpakt met `TimeZoneInfo` als je rond tijdzones moet dansen.

Naast `DateTime` hebben we `DateTimeOffset`. Het paart de datum-tijd aan een offset van UTC, handig als specificiteit van de tijdzone jouw ding is.

Wat implementatie betreft, `DateTime` in C# is nauwkeurig tot 100-nanoseconde tikken sinds middernacht, 1 januari 0001 n.Chr. Maar plan je nanoseconden er niet omheen – de nauwkeurigheid en precisie van de systeemklok variëren sterk.

## Zie Ook

- [DateTime Struct](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime?view=net-7.0)
- [DateTime.UtcNow Eigenschap](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetime.utcnow?view=net-7.0)
- [DateTimeOffset Struct](https://docs.microsoft.com/nl-nl/dotnet/api/system.datetimeoffset?view=net-7.0)
- [TimeZoneInfo Klasse](https://docs.microsoft.com/nl-nl/dotnet/api/system.timezoneinfo?view=net-7.0)
