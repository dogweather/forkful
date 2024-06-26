---
date: 2024-01-20 17:33:06.758108-07:00
description: "How to: Att j\xE4mf\xF6ra tv\xE5 datum i C# var inte alltid lika enkelt.\
  \ P\xE5 .NET:s tidigare dagar beh\xF6vde man kanske g\xF6ra fler steg och ber\xE4\
  kningar f\xF6r hand. Med\u2026"
lastmod: '2024-04-05T22:50:52.224779-06:00'
model: gpt-4-1106-preview
summary: "Att j\xE4mf\xF6ra tv\xE5 datum i C# var inte alltid lika enkelt."
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

## How to:
```C#
using System;

class DateComparison
{
    static void Main()
    {
        DateTime startDate = new DateTime(2023, 3, 1);
        DateTime endDate = new DateTime(2023, 3, 15);

        // Jämför två datum
        int comparison = DateTime.Compare(startDate, endDate);
        
        // Kolla vilket datum som är tidigast
        if(comparison < 0)
            Console.WriteLine("StartDate är före EndDate.");
        else if(comparison > 0)
            Console.WriteLine("StartDate är efter EndDate.");
        else
            Console.WriteLine("Datumen är desamma.");

        // Beräkna antal dagar mellan
        TimeSpan duration = endDate - startDate;
        Console.WriteLine($"Det är {duration.Days} dagar mellan start- och slutdatum.");
    }
}
```
Sample output:
```
StartDate är före EndDate.
Det är 14 dagar mellan start- och slutdatum.
```

## Deep Dive:
Att jämföra två datum i C# var inte alltid lika enkelt. På .NET:s tidigare dagar behövde man kanske göra fler steg och beräkningar för hand. Med `DateTime`-klassen och dess `Compare`-metod samt överlastning av operatorer som `-`, är det betydligt smidigare numera.

Du har alternativ som `TimeSpan` för att förenkla tidsspanner, eller att använda tredjepartspaket som NodaTime för ännu fler funktioner. För att hålla koden enkel har vi dock hållit oss till standardklassbiblioteket.

Detaljer som tidzoner och skottsekunder kan påverka jämförelser och tidsberäkningar, men för enkelhetens skull håller vi oss till `DateTime` som antar "lokaltid" om inget annat anges.

## See Also:
- [Microsoft Docs: DateTime.Compare Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare)
- [Microsoft Docs: TimeSpan Structure](https://docs.microsoft.com/en-us/dotnet/api/system.timespan)
- [NodaTime Documentation](https://nodatime.org/3.0.x/userguide)
- [Time and Date Programming in .NET Blog Post](https://devblogs.microsoft.com/dotnet/date-time-and-time-zone-enhancements-in-net-6/)
