---
date: 2024-01-20 17:30:50.150860-07:00
description: "Hur g\xF6r man: Tidigare anv\xE4nde programmerare ofta egna algoritmer\
  \ f\xF6r att ber\xE4kna tid, vilket kunde leda till felaktigheter p\xE5 grund av\
  \ komplexiteten i\u2026"
lastmod: '2024-04-05T22:50:52.225888-06:00'
model: gpt-4-1106-preview
summary: "Tidigare anv\xE4nde programmerare ofta egna algoritmer f\xF6r att ber\xE4\
  kna tid, vilket kunde leda till felaktigheter p\xE5 grund av komplexiteten i tidshantering,\
  \ t.ex."
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
weight: 26
---

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
