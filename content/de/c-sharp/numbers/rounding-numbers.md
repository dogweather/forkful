---
date: 2024-01-26 03:43:15.680281-07:00
description: "Wie: Hier ist Ihr Rundreiseticket f\xFCr das Runden von Zahlen in C#."
lastmod: '2024-03-13T22:44:53.883111-06:00'
model: gpt-4-0125-preview
summary: "Hier ist Ihr Rundreiseticket f\xFCr das Runden von Zahlen in C#."
title: Zahlen runden
weight: 13
---

## Wie:
Hier ist Ihr Rundreiseticket für das Runden von Zahlen in C#:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // Auf nächste ganze Zahl runden
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // Ausgabe: 123

        // Anzahl der Dezimalstellen angeben
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // Ausgabe: 123.46

        // Unabhängig von der nächsten Ziffer aufrunden
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // Ausgabe: 124

        // Unabhängig von der nächsten Ziffer abrunden
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // Ausgabe: 123
    }
}
```

## Vertiefung
Früher war das Runden ein Kinderspiel zur Reduzierung der Rechenkosten. Jeder Zyklus zählte, und das Kürzen von Zahlen sparte kostbare Zeit. Springen wir in die moderne Welt von C#, geht es um das Management des berüchtigten Hangs von Doubles und Decimals zu Präzisionsfehlern und Darstellungsproblemen.

Über `Math.Round`, `Math.Floor` und `Math.Ceiling` hinaus ermöglicht uns das Enum `MidpointRounding` das Schicksal von armen, mittig sitzenden Ziffern zu bestimmen – es ist der Scheideweg zwischen Bankregeln und der Spielplatzgerechtigkeit des „Aufrunden zur Hälfte“.

Für härtere Fälle, wie ernsthafte Mathematik- oder Finanzanwendungen, haben wir `decimal` statt `double`, was das Rundungsdrama durch eine höhere Präzision minimiert – weniger Rundungen, weniger Probleme.

## Siehe auch
- [Offizielle C#-Dokumentation zu `Math.Round`](https://docs.microsoft.com/de-de/dotnet/api/system.math.round)
- [Stack Overflow: Wann sollte ich Double statt Decimal verwenden?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [IEEE-Standard für Gleitkommaarithmetik (IEEE 754)](https://de.wikipedia.org/wiki/IEEE_754)
