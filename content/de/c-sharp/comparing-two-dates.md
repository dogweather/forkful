---
title:                "C#: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Manchmal musst du in deinem Code zwei Daten vergleichen, um beispielsweise festzustellen, ob eine bestimmte Aktion ausgeführt werden soll oder nicht. In diesem Blogpost werde ich dir zeigen, wie du dies einfach in C# tun kannst.

## How To

Das Vergleichen von Daten in C# ist ziemlich einfach und die Sprache bietet uns verschiedene Möglichkeiten, dies zu tun. Eine Möglichkeit ist die Verwendung des Operatoren "==" und "!=", die für Gleichheit und Ungleichheit stehen.

```C#
// Erstelle zwei Datumsvariablen
DateTime date1 = new DateTime(2021, 01, 01);
DateTime date2 = new DateTime(2020, 01, 01);

// Vergleiche die beiden Daten mit dem Operator "=="
if (date1 == date2)
{
    Console.WriteLine("Die Daten sind gleich.");
}

// Vergleiche die beiden Daten mit dem Operator "!="
if (date1 != date2)
{
    Console.WriteLine("Die Daten sind ungleich.");
}
```

Die Ausgabe dieses Codes wird "Die Daten sind ungleich." sein, da date1 und date2 unterschiedliche Jahreszahlen haben. 

Eine andere Möglichkeit, zwei Daten in C# zu vergleichen, ist die Verwendung der Methode "DateTime.Compare()" wie im folgenden Beispiel gezeigt:

```C#
// Verwende die Compare-Methode, um die Daten zu vergleichen
if (DateTime.Compare(date1, date2) > 0)
{
    Console.WriteLine("date1 ist nach date2.");
}
else if (DateTime.Compare(date1, date2) < 0)
{
    Console.WriteLine("date1 ist vor date2.");
}
else
{
    Console.WriteLine("Die Daten sind gleich.");
}
```

Die Ausgabe dieses Codes wird "date1 ist nach date2." sein, da die Methode "Compare" einen Wert zurückgibt, der angibt, ob date1 nach date2 liegt (größer als 0), vor date2 (kleiner als 0) oder gleich (0) ist.

## Deep Dive

Wenn du dich tiefer mit dem Vergleichen von Daten in C# beschäftigen möchtest, gibt es einige Dinge, die du beachten solltest. Zum Beispiel können DateTime-Variablen Zeitinformationen enthalten, die beim Vergleichen berücksichtigt werden müssen. Außerdem gibt es in C# verschiedene Methoden, um spezifische Datums- oder Zeitwerte zu vergleichen, wie z.B. "DateTime.Equals()" oder "DateTime.CompareTo()".

Ein weiterer wichtiger Punkt ist, dass C# nicht wie manch andere Programmiersprachen automatisch zwischen unterschiedlichen Datumsformaten konvertiert. Deshalb musst du bei der Verwendung von Variablen oder Strings, die ein Datum enthalten sollen, darauf achten, dass die Formate übereinstimmen.

## Siehe auch

- [Microsoft-Dokumentation: Datums- und Uhrzeitwerte vergleichen](https://docs.microsoft.com/de-de/dotnet/standard/datetime/comparing-datetime-values)
- [C#-Tipps: Datumsvergleich in C#](https://cstipps.wordpress.com/2016/07/11/datumsvergleich-in-c/)
- [Codebeispiele: Vergleichen von Datums- und Uhrzeitwerten in C#](https://www.red-gate.com/simple-talk/dotnet/net-framework/comparing-dates-time-vba-net-c/)