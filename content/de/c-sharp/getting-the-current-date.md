---
title:                "C#: Das aktuelle Datum erhalten"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Welt der Programmierung gibt es viele verschiedene Szenarien, in denen Sie möglicherweise das aktuelle Datum und die aktuelle Uhrzeit benötigen. Dies kann für die Verfolgung von Transaktionen, das Planen von Aufgaben oder sogar für die Ausführung von automatisierten Aktionen erforderlich sein. Egal aus welchem ​​Grund, das Abrufen des aktuellen Datums ist eine wichtige Funktion, die jeder Programmierer beherrschen sollte.

## Wie man das aktuelle Datum bekommt

Um das aktuelle Datum in C# zu bekommen, können Sie die integrierte `DateTime` Klasse verwenden. Diese Klasse enthält verschiedene Methoden, die es Ihnen ermöglichen, das aktuelle Datum auf verschiedene Weise abzurufen. Hier sind einige Beispiele:

````C#
// Gibt das aktuelle Datum und die aktuelle Uhrzeit zurück
DateTime now = DateTime.Now;
Console.WriteLine(now);
// Output: 3/31/2021 11:24:06 AM

// Gibt das aktuelle Datum zurück
DateTime today = DateTime.Today;
Console.WriteLine(today);
// Output: 3/31/2021

// Gibt die aktuelle Uhrzeit zurück
DateTime currentTime = DateTime.Now.TimeOfDay;
Console.WriteLine(currentTime);
// Output: 11:24:06.6672805
````

Wie Sie sehen, können Sie mit der `DateTime` Klasse das aktuelle Datum und die aktuelle Uhrzeit in verschiedenen Formaten abrufen. Sie können auch spezifizieren, in welcher Zeitzone Sie das Datum abrufen möchten. Wenn Sie beispielsweise ein Programm für Benutzer auf der ganzen Welt schreiben, ist es wichtig, die Zeitzone zu berücksichtigen, damit das Datum und die Uhrzeit korrekt angezeigt werden.

## Deep Dive

Wenn Sie tiefer in die Welt des Datums und der Zeit in C# eintauchen möchten, können Sie auch die `DateTimeOffset` und `TimeZoneInfo` Klassen erkunden. Diese bieten weitere Funktionen und Möglichkeiten, um das Datum und die Uhrzeit zu verarbeiten.

Eine wichtige Sache, die Sie beachten sollten, ist, dass das aktuelle Datum und die Uhrzeit immer auf dem lokalen Computer basieren. Wenn Sie sich auf verschiedenen Computern in verschiedenen Zeitzonen befinden, können das Datum und die Uhrzeit abweichen. Es ist daher wichtig, dies bei der Verarbeitung von Datum und Uhrzeit in Ihrem Code zu berücksichtigen.

## Siehe auch

- Offizielle Dokumentation für die `DateTime` Klasse in C#: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0
- Ein Tutorial für die Verarbeitung von Datum und Uhrzeit in C#: https://www.tutorialspoint.com/csharp/csharp_date_time.htm
- Eine Liste von häufig gestellten Fragen zu Datum und Uhrzeit in C#: https://www.c-sharpcorner.com/blogs/how-to-work-with-date-and-time-in-c-sharp-c-sharp-corner-faq1