---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:25.562023-07:00
description: "Att f\xE5 det aktuella datumet i C# inneb\xE4r att h\xE4mta aktuella\
  \ datum- och tidsuppgifter fr\xE5n systemet. Programmerare beh\xF6ver ofta tillg\xE5\
  ng till denna\u2026"
lastmod: '2024-03-13T22:44:37.923748-06:00'
model: gpt-4-0125-preview
summary: "Att f\xE5 det aktuella datumet i C# inneb\xE4r att h\xE4mta aktuella datum-\
  \ och tidsuppgifter fr\xE5n systemet. Programmerare beh\xF6ver ofta tillg\xE5ng\
  \ till denna\u2026"
title: "F\xE5 det aktuella datumet"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få det aktuella datumet i C# innebär att hämta aktuella datum- och tidsuppgifter från systemet. Programmerare behöver ofta tillgång till denna information för loggning, tidsstämpling av operationer eller schemaläggning av uppgifter inom applikationer, för att säkerställa att handlingar är tidsmässigt korrekta och data är markerat med exakta tidsstämplar.

## Hur man gör:
C# erbjuder ett enkelt sätt att få det aktuella datumet genom att använda `DateTime`-klassen som är en del av .NET Frameworks System-namnrymd. Exemplet nedan demonstrerar hur man får det aktuella datumet, och eventuellt, tiden.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Får endast det aktuella datumet
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Utdata: MM/dd/yyyy
        
        // Får det aktuella datumet och tiden
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Utdata: MM/dd/yyyy HH:mm:ss

        // Får det aktuella UTC-datumet och tiden
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Utdata: MM/dd/yyyy HH:mm:ss
    }
}
```

När det gäller tredjepartsbibliotek erbjuder NodaTime ett robust alternativ för manipulation av datum och tid, inklusive hämtning av det aktuella datumet i olika kalendrar och tidszoner.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Använder NodaTime för att få det aktuella datumet i ISO-kalendern
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Utdata: yyyy-MM-dd

        // För tidszonspecifika datum
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Utdata: yyyy-MM-dd
    }
}
```

Detta visar den grundläggande användningen med den inbyggda `DateTime`-klassen och de förbättrade möjligheterna som NodaTime erbjuder, särskilt användbart för applikationer som kräver hantering av olika tidszoner eller kalendersystem.
