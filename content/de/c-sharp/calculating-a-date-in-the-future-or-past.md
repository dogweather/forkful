---
title:                "C#: Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann nützlich sein, um bestimmte Ereignisse oder Termine im Voraus zu planen. Auch in der Datenverarbeitung und Programmierung kann die Berechnung von Datumswerten eine wichtige Rolle spielen.

## Wie

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir in C# die DateTime-Klasse verwenden. Wir müssen lediglich das aktuelle Datum als Startpunkt festlegen und dann eine bestimmte Anzahl von Tagen, Monaten oder Jahren hinzufügen oder subtrahieren.

```C#
// Aktuelles Datum erhalten
DateTime now = DateTime.Now;

// Datum in der Zukunft berechnen
DateTime futureDate = now.AddDays(7); // 7 Tage zukünftig

// Datum in der Vergangenheit berechnen
DateTime pastDate = now.AddDays(-3); // 3 Tage zurückliegend

// Ausgabe der berechneten Datumswerte
Console.WriteLine("Zukünftiges Datum: " + futureDate.ToString("dd.MM.yyyy"));
Console.WriteLine("Vergangenes Datum: " + pastDate.ToString("dd.MM.yyyy"));
```

Die obigen Beispiele zeigen, wie wir die AddDays-Methode verwenden können, um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Dies kann auch mit Monaten (AddMonths) oder Jahren (AddYears) erfolgen. Wir können auch jede beliebige Anzahl von Tagen, Monaten oder Jahren angeben, um das Datum zu berechnen.

## Deep Dive

Die DateTime-Klasse bietet auch viele weitere nützliche Methoden zur Berechnung von Datumswerten, wie zum Beispiel das Festlegen von bestimmten Wochentagen (SetDayOfWeek) oder das Ermitteln der Differenz zwischen zwei Daten (Subtract). Auch die Verwendung von DateTimeOffset, anstelle von DateTime, ermöglicht eine genauere Erfassung von Datum und Uhrzeit mit Zeitzonen.

Es ist auch wichtig zu beachten, dass das Datum in verschiedenen Kulturen unterschiedlich formatiert werden kann. In C# können wir dies berücksichtigen, indem wir die Kultur (CultureInfo) bei der Ausgabe des berechneten Datums angeben.

## Siehe auch

- [Microsoft-Dokumentation zur DateTime-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=net-5.0)
- [C#-Beispielsammlung für die DateTime-Klasse](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/classes-and-structs/how-to-use-datetime-values)
- [Tutorial zum Umgang mit Zeit und Datum in C#](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)