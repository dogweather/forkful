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

## Warum
Vergleichen von zwei Daten kann nützlich sein, um festzustellen, ob sie gleich oder unterschiedlich sind. Dies kann besonders wichtig sein, wenn man mit Datumsangaben in Programmen arbeitet, wie z.B. bei der Verarbeitung von Nutzeranfragen oder beim Filtern von Daten.

## How To
Um zwei Daten in C# zu vergleichen, verwenden wir die eingebaute `DateTime` Klasse. Hier ist ein einfaches Beispiel, bei dem wir zwei Daten erstellen und dann überprüfen, ob sie gleich sind:

```C#
DateTime datum1 = new DateTime(2021, 6, 15);
DateTime datum2 = new DateTime(2021, 6, 15);

if(datum1 == datum2)
{
  Console.WriteLine("Die Daten sind gleich!");
}
else
{
  Console.WriteLine("Die Daten sind unterschiedlich.");
}
```

Die Ausgabe dieses Codes ist "Die Daten sind gleich!", da beide `DateTime` Objekte das gleiche Datum enthalten.

Wir können auch verschiedene Methoden der `DateTime` Klasse verwenden, um spezifische Eigenschaften der Daten zu vergleichen. Zum Beispiel können wir die `Compare` Methode verwenden, um zu prüfen, ob ein Datum vor oder nach einem anderen Datum liegt:

```C#
if(DateTime.Compare(datum1, datum2) > 0)
{
  Console.WriteLine("datum1 liegt nach datum2.");
}
else if(DateTime.Compare(datum1, datum2) < 0)
{
  Console.WriteLine("datum1 liegt vor datum2.");
}
else
{
  Console.WriteLine("Die Daten sind gleich.");
}
```

In diesem Beispiel wird das Programm überprüfen, ob `datum1` nach `datum2` liegt und gibt dies entsprechend aus. Wenn die Daten gleich sind, wird die letzte Bedingung ausgeführt.

## Deep Dive
Die `DateTime` Klasse in C# bietet verschiedene Methoden und Eigenschaften, mit denen wir detailliertere Vergleiche zwischen zwei Daten durchführen können. Zum Beispiel können wir die `Ticks` Eigenschaft verwenden, um die Anzahl der Ticks (100-nanosekundige Intervalle) zu vergleichen, die seit dem 1. Januar 0001 vergangen sind.

```C#
if(datum1.Ticks > datum2.Ticks)
{
  Console.WriteLine("datum1 liegt nach datum2.");
}
else if(datum1.Ticks < datum2.Ticks)
{
  Console.WriteLine("datum1 liegt vor datum2.");
}
else
{
  Console.WriteLine("Die Daten sind gleich.");
}
```

Es ist auch möglich, die `DateTimeOffset` Klasse zu verwenden, um zwei Daten mit expliziten Zeitzonenangaben zu vergleichen. Dies kann wichtig sein, wenn wir mit Daten aus verschiedenen Zeitzonen arbeiten.

```C#
DateTimeOffset dateOffset1 = new DateTimeOffset(2021, 6, 15, 10, 30, 0, new TimeSpan(+2, 0, 0));
DateTimeOffset dateOffset2 = new DateTimeOffset(2021, 6, 15, 8, 30, 0, new TimeSpan(-5, 0, 0));

if(dateOffset1 > dateOffset2)
{
  Console.WriteLine("dateOffset1 liegt nach dateOffset2.");
}
else if(dateOffset1 < dateOffset2)
{
  Console.WriteLine("dateOffset1 liegt vor dateOffset2.");
}
else
{
  Console.WriteLine("Die Daten sind gleich.");
}
```

## Siehe auch
- Microsoft Docs: [DateTime Struktur (C#)](https://docs.microsoft.com/de-de/dotnet/api/system.datetime)
- Microsoft Docs: [DateTime.Compare Methode (C#)](https://docs.microsoft.com/de-de/dotnet/api/system.datetime.compare)
- Microsoft Docs: [DateTimeOffset Struktur (C#)](https://docs.microsoft.com/de-de/dotnet/api/system.datetimeoffset)