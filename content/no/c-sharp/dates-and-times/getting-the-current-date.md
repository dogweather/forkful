---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:15.826089-07:00
description: "Hvordan: C# tilbyr en enkel m\xE5te \xE5 f\xE5 dagens dato p\xE5 ved\
  \ bruk av `DateTime`-klassen som er en del av .NET Frameworks System-navnerom. Eksemplet\
  \ nedenfor\u2026"
lastmod: '2024-03-13T22:44:40.807302-06:00'
model: gpt-4-0125-preview
summary: "C# tilbyr en enkel m\xE5te \xE5 f\xE5 dagens dato p\xE5 ved bruk av `DateTime`-klassen\
  \ som er en del av .NET Frameworks System-navnerom."
title: "F\xE5 dagens dato"
weight: 29
---

## Hvordan:
C# tilbyr en enkel måte å få dagens dato på ved bruk av `DateTime`-klassen som er en del av .NET Frameworks System-navnerom. Eksemplet nedenfor demonstrerer hvordan du kan få dagens dato, og eventuelt, tiden.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Får kun dagens dato
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Utdata: MM/dd/yyyy
        
        // Får dagens dato og tid
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Utdata: MM/dd/yyyy HH:mm:ss

        // Får dagens UTC dato og tid
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Utdata: MM/dd/yyyy HH:mm:ss
    }
}
```

Når det gjelder tredjepartsbiblioteker, tilbyr NodaTime et robust alternativ for manipulering av dato og tid, inkludert å hente dagens dato i ulike kalendere og tidssoner.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Bruker NodaTime for å få dagens dato i ISO-kalenderen
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Utdata: yyyy-MM-dd

        // For tidssone-spesifikke datoer
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Utdata: yyyy-MM-dd
    }
}
```

Dette viser grunnleggende bruk med den innebygde `DateTime`-klassen og de forbedrede evnene som tilbys av NodaTime, spesielt nyttig for applikasjoner som krever håndtering av ulike tidssoner eller kalendersystemer.
