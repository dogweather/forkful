---
date: 2024-01-20 17:28:34.621911-07:00
description: "\xC5 regne ut en dato i fremtiden eller fortiden handler om \xE5 finne\
  \ en dato f\xF8r eller etter et gitt tidspunkt. Programmerere gj\xF8r dette for\
  \ \xE5 h\xE5ndtere\u2026"
lastmod: '2024-03-13T22:44:40.810833-06:00'
model: gpt-4-1106-preview
summary: "\xC5 regne ut en dato i fremtiden eller fortiden handler om \xE5 finne en\
  \ dato f\xF8r eller etter et gitt tidspunkt."
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## How to:
Håndtere datoer i C# er rett frem. Bruk `DateTime` og `TimeSpan` klassene. Se eksemplene:

```C#
DateTime today = DateTime.Now;
TimeSpan tenDays = TimeSpan.FromDays(10);

// Dato i fremtiden
DateTime futureDate = today.AddDays(10);
Console.WriteLine($"Om 10 dager: {futureDate.ToShortDateString()}"); // Output: Om 10 dager: [dato 10 dager fra nå]

// Dato i fortiden
DateTime pastDate = today.AddDays(-10);
Console.WriteLine($"For 10 dager siden: {pastDate.ToShortDateString()}"); // Output: For 10 dager siden: [dato 10 dager før nå]
```

## Deep Dive
Før `DateTime`, måtte programmerere håndtere datoer ved hjelp av primitive datatyper og egne funksjoner. `DateTime` forenkler prosessen enormt. Alternativer inkluderer NodaTime-biblioteket for komplekse tidsoperasjoner og DateTimeOffset for tidssonebehandling. Når du regner med datoer, husk leap år og tidszoner. Med `DateTime`, håndteres de fleste av disse detaljene for deg.

## See Also
- [Microsoft's DateTime documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [TimeSpan documentation](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-6.0)
- [NodaTime](https://nodatime.org/)
- [DateTimeOffset documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-6.0)
