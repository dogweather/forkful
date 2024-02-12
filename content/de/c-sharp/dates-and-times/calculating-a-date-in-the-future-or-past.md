---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
aliases: - /de/c-sharp/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:28:33.483973-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Berechnungen von zukünftigen oder vergangenen Daten ermöglichen es, Zeitabstände zu erfassen, wie zum Beispiel Liefertermine oder Jubiläen. Programmierer nutzen diese Funktionen, um Zeitspannen zu verwalten, Erinnerungen zu setzen und Termine zu planen.

## How to:

Zukünftige und vergangene Daten können in C# einfach mit der `DateTime`-Klasse berechnet werden. Hier ein paar Beispiele:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime today = DateTime.Now;
        
        // Berechnung eines zukünftigen Datums (10 Tage ab heute)
        DateTime futureDate = today.AddDays(10);
        Console.WriteLine($"Zukünftiges Datum: {futureDate.ToShortDateString()}"); // Zum Beispiel: 20.04.2023

        // Berechnung eines vergangenen Datums (30 Tage vor heute)
        DateTime pastDate = today.AddDays(-30);
        Console.WriteLine($"Vergangenes Datum: {pastDate.ToShortDateString()}"); // Zum Beispiel: 12.03.2023
    }
}
```

Sample Output:

```
Zukünftiges Datum: 20.04.2023
Vergangenes Datum: 12.03.2023
```

## Deep Dive:

Die Klasse `DateTime` wurde mit .NET Framework eingeführt und ist bis heute zentral für die Zeit- und Datumshandhabung in C#. Alternativen wie `TimeSpan` ermöglichen Differenzberechnungen zwischen zwei Daten. Seit .NET Core gibt es zusätzlich `DateOnly` und `TimeOnly` für spezifischere Anwendungen.

Das einfache Addieren und Subtrahieren von Tagen ist intuitiv, allerdings gibt es viele Details zu beachten, wie Zeitzonen und Schaltjahre. Libraries wie NodaTime bieten erweiterte Funktionen für komplexe Zeitberechnungen und sollten für entsprechend anspruchsvolle Anwendungen in Betracht gezogen werden.

Generell sollte bei der Zeit- und Datumsberechnung immer darauf geachtet werden, dass die Ergebnisse von der gesetzten Kultur (`CultureInfo`) und der Zeitzone des Systems abhängen können.

## See Also:

Für mehr Informationen zu `DateTime` und Zeitberechnungen in .NET, sieh dir folgende Ressourcen an:

- [DateTime Struktur](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-6.0)
- [TimeSpan Struktur](https://docs.microsoft.com/de-de/dotnet/api/system.timespan?view=net-6.0)
- [NodaTime](https://nodatime.org/)
- [Arbeiten mit Zeitzonen](https://docs.microsoft.com/de-de/dotnet/standard/datetime/working-with-calendars)
