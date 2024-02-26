---
date: 2024-01-20 17:30:50.150860-07:00
description: "Ber\xE4kning av ett datum i framtiden eller det f\xF6rflutna inneb\xE4\
  r att ta ett specifikt datum och addera eller subtrahera tid fr\xE5n det. Programmerare\
  \ g\xF6r\u2026"
lastmod: '2024-02-25T18:49:36.225229-07:00'
model: gpt-4-1106-preview
summary: "Ber\xE4kning av ett datum i framtiden eller det f\xF6rflutna inneb\xE4r\
  \ att ta ett specifikt datum och addera eller subtrahera tid fr\xE5n det. Programmerare\
  \ g\xF6r\u2026"
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av ett datum i framtiden eller det förflutna innebär att ta ett specifikt datum och addera eller subtrahera tid från det. Programmerare gör detta för att hantera bokningar, påminnelser eller giltighetstider i mjukvaruapplikationer.

## Hur gör man:
```C#
using System;

class Program {
    static void Main() {
        DateTime originalDate = DateTime.Now;
        TimeSpan tenDays = TimeSpan.FromDays(10);
        DateTime futureDate = originalDate.Add(tenDays);
        DateTime pastDate = originalDate.Subtract(tenDays);
        
        Console.WriteLine($"Idag: {originalDate.ToShortDateString()}");
        Console.WriteLine($"Framtida datum: {futureDate.ToShortDateString()}");
        Console.WriteLine($"Förflutet datum: {pastDate.ToShortDateString()}");
    }
}
```
**Exempel på utdata:**
```
Idag: 08/04/2023
Framtida datum: 18/04/2023
Förflutet datum: 29/03/2023
```

## Djupdykning:
Tidigare använde programmerare ofta egna algoritmer för att beräkna tid, vilket kunde leda till felaktigheter på grund av komplexiteten i tidshantering, t.ex. skottår och tidszoner. I C# förenklas detta genom `DateTime` och `TimeSpan`-klasserna.

Alternativ till detta är tredjepartsbibliotek som NodaTime som erbjuder mer robust tidshantering, särskilt i komplexa scenarier.

Implementationens nyckeldetaljer inkluderar hantering av skottsekunder och lokala versus universella tidszoner. C# hanterar detta automatiskt, vilket minskar risken för misstag.

## Se även:
- [Microsoft DateTime Dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [TimeSpan Microsoft Dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-6.0)
- [NodaTime Dokumentation](https://nodatime.org/)
