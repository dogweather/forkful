---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:08.783435-07:00
description: "Das Abrufen des aktuellen Datums in C# beinhaltet das Abrufen der aktuellen\
  \ Datums- und Uhrzeitangaben vom System. Programmierer m\xFCssen oft auf diese\u2026"
lastmod: '2024-03-11T00:14:27.794690-06:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in C# beinhaltet das Abrufen der aktuellen\
  \ Datums- und Uhrzeitangaben vom System. Programmierer m\xFCssen oft auf diese\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in C# beinhaltet das Abrufen der aktuellen Datums- und Uhrzeitangaben vom System. Programmierer müssen oft auf diese Informationen zugreifen, um Vorgänge zu protokollieren, Aktionen zu zeitstempeln oder Aufgaben innerhalb von Anwendungen zu planen, um sicherzustellen, dass Handlungen genau getimt und Daten mit präzisen Zeitstempeln versehen werden.

## Wie:
C# bietet eine unkomplizierte Methode, um das aktuelle Datum mittels der `DateTime` Klasse zu erhalten, die Teil des System-Namensraums des .NET-Frameworks ist. Das folgende Beispiel demonstriert, wie man das aktuelle Datum und optional die Uhrzeit erhält.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Holt nur das aktuelle Datum
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Ausgabe: MM/dd/yyyy
        
        // Holt das aktuelle Datum und die Uhrzeit
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Ausgabe: MM/dd/yyyy HH:mm:ss

        // Holt das aktuelle UTC-Datum und die Uhrzeit
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Ausgabe: MM/dd/yyyy HH:mm:ss
    }
}
```

Was Drittanbieter-Bibliotheken betrifft, bietet NodaTime eine robuste Alternative für die Manipulation von Datum und Uhrzeit, einschließlich des Abrufens des aktuellen Datums in verschiedenen Kalendern und Zeitzonen.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Verwendung von NodaTime, um das aktuelle Datum im ISO-Kalender zu erhalten
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Ausgabe: yyyy-MM-dd

        // Für zeitzonenspezifische Daten
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Ausgabe: yyyy-MM-dd
    }
}
```

Dies veranschaulicht die grundlegende Verwendung mit der integrierten `DateTime` Klasse und die erweiterten Fähigkeiten, die durch NodaTime bereitgestellt werden, besonders nützlich für Anwendungen, die die Verwaltung verschiedener Zeitzonen oder Kalendersysteme erfordern.
