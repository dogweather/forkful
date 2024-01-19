---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vergleich von zwei Daten in C#

## Was & Warum?

Der Vergleich von zwei Daten bedeutet, zu sehen, welches Datum früher oder später ist oder ob sie gleich sind. Programmiere tun dies, um zeitbasierte Logik in ihren Anwendungen zu implementieren.

## So geht's:

Du kannst zwei Daten in C# vergleichen, indem du die DateTime-Objekte und die CompareTo-Methode verwendest. Hier ist ein Code-Beispiel:

```csharp
DateTime datum1 = new DateTime(2021, 12, 15);
DateTime datum2 = new DateTime(2022, 1, 1);

int ergebnis = datum1.CompareTo(datum2);

if (ergebnis < 0)
   Console.WriteLine("datum1 is früher als datum2");
else if (ergebnis > 0)
   Console.WriteLine("datum1 is später als datum2");
else
   Console.WriteLine("beide Daten sind gleich");
```

Dieser Code gibt aus: "datum1 ist früher als datum2".

## Vertiefung

Bei der Arbeit mit Daten in C# haben wir schon immer die DateTime-Klasse verwendet. In älteren Versionen von .NET und C# gab es keine integrierte Möglichkeit, Daten zu vergleichen. Man musste das Jahr, den Monat und den Tag manuell vergleichen.

Heutzutage bietet .NET mehrere Möglichkeiten zum Vergleich von Daten. Neben der CompareTo-Methode, die wir gerade gesehen haben, gibt es auch die statische Methode Compare von DateTime, die eine sehr ähnliche Funktionalität bietet.

Unabhängig von der Methode, die du verwendest, musst du bedenken, dass der Vergleich auf Grundlage der DateTime-Objekte erfolgt. Deshalb ist es wichtig, sicherzustellen, dass diese korrekt initialisiert sind, bevor du sie vergleichst.

## Siehe auch:

Für weitere Informationen zum Umgang mit Daten in C# könnten folgende Links hilfreich sein:

- [DateTime Struktur](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-5.0)
- [Vergleich von Daten und Zeiten](https://docs.microsoft.com/de-de/dotnet/standard/base-types/comparing-dates-and-times)
- [Arbeiten mit Daten in .NET](https://docs.microsoft.com/de-de/dotnet/standard/datetime)