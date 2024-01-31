---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:13:45.866240-07:00
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist eine Basics. Ob du nun einen Zeitstempel setzen, eine Zeitspanne berechnen oder den Nutzer wissen lassen willst, was heute für ein Tag ist – das Datum verrät es dir.

## How to:
Um das aktuelle Datum in C# zu holen, nutzt du `DateTime.Now`. Hier ist der schnelle Weg:

```C#
using System;

public class CurrentDateExample
{
    static void Main(string[] args)
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate.ToString("d")); // Kurzes Datum
        Console.WriteLine(currentDate.ToString("g")); // Datum mit Uhrzeit
    }
}
```
Das gibt dir sowas hier aus:

```
31.03.2023
31.03.2023 14:07
```

## Deep Dive:
`DateTime.Now` gehört zum .NET Framework seit Version 1.0 – ein echter Dinosaurier also. Es zieht die Systemuhr heran, um dir Datum und Uhrzeit zu liefern. Früher waren Alternativen wie `DateTime.UtcNow` für Weltzeit (UTC) wichtig, um Zeitzone-Durcheinander zu vermeiden. Mit .NET 6 kam `DateOnly` dazu, wenn du echt nur das Datum brauchst, ohne die Zeit.

Die Implementierung nutzt die systemeigenen APIs deines Betriebssystems. Das ist normalerweise superpräzise, kann sich aber verzetteln, wenn der Nutzer die Systemzeit manipuliert. Für kritische Anwendungen, wo Zuverlässigkeit ein Muss ist, solltest du eine vertrauenswürdige Zeitquelle einbinden – etwa einen NTP-Server.

## See Also:
- Microsoft-Doku für `DateTime`: [https://docs.microsoft.com/dotnet/api/system.datetime](https://docs.microsoft.com/dotnet/api/system.datetime)
- Überlegungen zur Zeitzone: [https://docs.microsoft.com/dotnet/standard/datetime/choosing-between-datetime](https://docs.microsoft.com/dotnet/standard/datetime/choosing-between-datetime)
- Infos zu `DateOnly` und `TimeOnly`: [https://devblogs.microsoft.com/dotnet/date-time-and-time-zone-enhancements-in-net-6/](https://devblogs.microsoft.com/dotnet/date-time-and-time-zone-enhancements-in-net-6/)
