---
title:                "Få dagens dato"
date:                  2024-02-03T19:09:15.826089-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få dagens dato"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få tak i dagens dato i C# innebærer å hente dagens dato- og tidsdetaljer fra systemet. Programmerere trenger ofte å få tilgang til denne informasjonen for logging, tidsstemplingsoperasjoner, eller planlegging av oppgaver innen applikasjoner, for å sikre at handlinger er tidsbestemte nøyaktig og data er merket med presise tidsstempler.

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
