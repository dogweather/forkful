---
title:    "C#: Das aktuelle Datum erhalten."
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Wahrscheinlich hat sich jeder schon mal gefragt, wie man in einer Programmiersprache wie C# das aktuelle Datum bekommen kann. Es gibt viele Anwendungsgebiete, in denen es wichtig ist, das aktuelle Datum zu kennen, zum Beispiel in der Entwicklung von Terminplanern, Aufgabenverwaltungssystemen oder Buchungsanwendungen. Aber auch in Alltagssituationen wie beim Erstellen von Logbucheinträgen oder beim Hinzufügen von Zeitstempeln zu Nachrichten ist es hilfreich, schnell und einfach das aktuelle Datum abrufen zu können. In diesem Blogbeitrag werden wir uns genau damit beschäftigen und zeigen, wie man in C# das aktuelle Datum bekommen kann.

## How To

Das aktuelle Datum in C# zu bekommen ist sehr einfach. Es gibt dafür eine integrierte Funktion namens "DateTime.Now", die das aktuelle Datum und die aktuelle Uhrzeit zurückliefert. Schauen wir uns dazu ein Beispiel an:

```C#
DateTime now = DateTime.Now; // aktuelles Datum und Uhrzeit abrufen
Console.WriteLine(now); // Datum und Uhrzeit auf der Konsole ausgeben
```

Die Ausgabe dieses Codes wäre zum Beispiel: 30.06.2021 15:30:00 (basierend auf dem aktuellen Datum und der aktuellen Uhrzeit). 

Man kann jedoch auch nur das Datum oder die Uhrzeit separat abrufen, zum Beispiel mit den Funktionen "DateTime.Today" (nur Datum) und "DateTime.TimeOfDay" (nur Uhrzeit). Hier ein Beispiel:

```C#
DateTime today = DateTime.Today; // aktuelles Datum abrufen
Console.WriteLine(today); // Datum auf der Konsole ausgeben
TimeSpan time = DateTime.TimeOfDay; // aktuelle Uhrzeit abrufen
Console.WriteLine(time); // Uhrzeit auf der Konsole ausgeben
```

Die Ausgabe dieses Codes wäre zum Beispiel: 30.06.2021 (für das aktuelle Datum) und 15:30:00 (für die aktuelle Uhrzeit).

Es gibt auch die Möglichkeit, das aktuelle Datum in einem bestimmten Format auszugeben, zum Beispiel nur als Tag und Monat oder nur als Jahr. Hier ein Beispiel:

```C#
DateTime now = DateTime.Now; // aktuelles Datum und Uhrzeit abrufen
Console.WriteLine(now.ToString("dd-MM")); // Ausgabe: 30-06 (Tag-Monat)
Console.WriteLine(now.ToString("yyyy")); // Ausgabe: 2021 (Jahr)
```

Es gibt zahlreiche Formatierungsmöglichkeiten, um das Datum in verschiedenen Variationen auszugeben. Es ist auch möglich, das Datum in eine andere Zeitzone umzurechnen, falls dies benötigt wird. Dafür gibt es die Funktion "DateTime.ToLocalTime()" oder "DateTime.ToUniversalTime()", je nachdem ob man die Zeit in die lokale Zeitzone oder in die UTC-Zeitzone umrechnen möchte.

## Deep Dive

In C# gibt es neben den oben genannten Funktionen noch weitere Möglichkeiten, um das aktuelle Datum zu bekommen und zu manipulieren. Eine davon ist die Funktion "DateTime.Parse()", mit der ein String in ein DateTime-Objekt umgewandelt werden kann. Hier ein Beispiel:

```C#
string date = "2021-06-30"; // Datum als String
DateTime parsedDate = DateTime.Parse(date); // String in DateTime-Objekt umwandeln
Console.WriteLine(parsedDate); // Ausgabe: 30.06.2021 00:00:00
```

Es gibt auch die Möglichkeit, das aktuelle Datum mit bestimmten Zeitzoneninformationen abzurufen. Dafür gibt es die Funktion "DateTimeOffset.Now", die ein DateTime-Objekt mit zusätzlichen Zeitzoneninformationen zurückgibt.

```C#
DateTimeOffset offset = DateTimeOffset.Now; // aktuelles Datum mit Zeitzoneninformationen abrufen
Console.WriteLine(offset); // Ausgabe: 30.06.2021 15:30:00 +02:00
```

Es gibt noch viele weitere Funktionen und Möglichkeiten, um das aktuelle Datum in C# abzurufen und zu manipulieren, jedoch sollte dies für die meisten Anwendungen ausreichen.

## Siehe auch

* [Microsoft Docs - DateTime Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-5.0)
* [Microsoft Docs - Standardformatzeichenfolgen für das Datum](https://docs.microsoft.com/de-de/dotnet/standard/base-types/standard-date-and-time-format-strings)