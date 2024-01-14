---
title:    "C#: Vergleich von zwei Datumsangaben"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
Das Vergleichen von zwei Datumsangaben ist für die Entwicklung von C#-Anwendungen unerlässlich. Es ermöglicht die Durchführung von Aufgaben wie Überprüfung von Ablaufdaten, Berechnung von Altersunterschieden und vieles mehr. In diesem Blogbeitrag werden wir uns ansehen, wie man in C# zwei Datumsangaben vergleichen kann.

## Wie geht es
Um zwei Datumsangaben zu vergleichen, benötigen wir das DateTime-Objekt in C#. Dieses Objekt stellt eine bestimmte Datum- und Zeitangabe dar. Nehmen wir an, wir haben zwei Datumsangaben, 1. Januar 2020 und 1. Januar 2021, die wir vergleichen möchten. Wir können dies folgendermaßen tun:

```C#
DateTime date1 = new DateTime(2020, 1, 1);
DateTime date2 = new DateTime(2021, 1, 1);
```

Die DateTime-Klasse verfügt über eine Methode namens CompareTo(), die verwendet werden kann, um zwei Datumsangaben miteinander zu vergleichen. Diese Methode gibt eine negative Zahl zurück, wenn das erste Datum kleiner als das zweite ist, eine positive Zahl, wenn es größer ist, und 0, wenn beide Datumsangaben gleich sind.

```C#
int result = date1.CompareTo(date2); // result = -1
```

Alternativ können wir auch die logischen Operatoren <=, >=, < oder > verwenden, um zwei Datumsangaben zu vergleichen.

```C#
bool result = date1 <= date2; // result = true
```

Ein weiteres nützliches Tool ist die Methode Equals(), die überprüft, ob zwei Datumsangaben gleich sind.

```C#
bool result = date1.Equals(date2); // result = false
```

## Tief einsteigen
Beim Vergleichen von zwei Datumsangaben müssen wir auch die Zeitzone berücksichtigen. In C# können wir dies erreichen, indem wir die Methode ToLocalTime() verwenden, die die angegebene Zeit in die lokale Zeitzone konvertiert.

```C#
DateTime date1 = new DateTime(2020, 1, 1, 10, 0, 0); // 1. Januar 2020 um 10:00 Uhr
DateTime date2 = new DateTime(2020, 1, 1, 9, 0, 0); // 1. Januar 2020 um 9:00 Uhr
bool result = date1.Equals(date2); // result = false
bool result2 = date1.ToLocalTime().Equals(date2.ToLocalTime()); // result2 = true
```

Es ist auch wichtig zu beachten, dass DateTime-Objekte in C# unveränderlich sind, was bedeutet, dass sie nicht direkt bearbeitet werden können. Stattdessen wird bei Änderungen ein neues DateTime-Objekt erstellt.

```C#
DateTime date1 = new DateTime(2020, 1, 1);
date1.AddDays(1); // date1 bleibt 1. Januar 2020
DateTime date2 = date1.AddDays(1); // date2 ist 2. Januar 2020
```

## Siehe auch
- [Microsoft-Dokumentation zu DateTime-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.datetime)
- [C#-Tutorial: Datum und Zeit](https://www.tutorialspoint.com/de/csharp/csharp_date_time.html)
- [Schwierigkeiten beim Vergleichen von Datum und Zeit in C#](https://stackoverflow.com/questions/8191576/unable-to-compare-datetime-values-in-c)