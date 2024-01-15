---
title:                "Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "C#: Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe dafür, ein Datum in der Zukunft oder Vergangenheit zu berechnen. Zum Beispiel könnte man eine Software schreiben, die automatisch an Geburtstage oder wichtige Termine erinnert. Oder man muss in einer Datenbank nach bestimmten Daten suchen, die ein bestimmtes Zeitintervall umfassen.

## Wie geht's
Der einfachste Weg, ein Datum in der Zukunft oder Vergangenheit zu berechnen, ist die Verwendung der `Add`-Methode auf einem vorhandenen `DateTime`-Objekt. Diese Methode nimmt eine `TimeSpan` als Parameter und gibt ein neues `DateTime`-Objekt zurück, das das ursprüngliche Datum um die angegebene Zeitspanne verschiebt.

```C#
// Berechne das Datum, das 10 Tage in der Zukunft liegt
DateTime zukunft = DateTime.Now.Add(new TimeSpan(10, 0, 0, 0));

// Berechne das Datum, das 2 Monate und 3 Tage in der Vergangenheit liegt
DateTime vergangenheit = DateTime.Now.Add(new TimeSpan(-2, -3, 0, 0));
```

Die `TimeSpan`-Klasse stellt verschiedene Konstruktoren zur Verfügung, um verschiedene Zeitintervalle zu definieren. Zum Beispiel könnte man die Anzahl von Tagen, Stunden, Minuten und Sekunden angeben oder die Gesamtzahl an Stunden oder Minuten. Eine vollständige Liste der verfügbaren Konstruktoren und Methoden findet man in der offiziellen [Dokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.timespan).

## Tiefen-Tauchgang
Die `DateTime`-Klasse enthält auch viele nützliche Methoden, um mit Datumsangaben zu arbeiten. Mit `DateTime.Today` erhält man das aktuelle Datum ohne die Uhrzeit, was für Vergleiche und Berechnungen praktisch ist. Mit `DateTime.Now` kann man die aktuelle Uhrzeit in die Berechnung einbeziehen. Um eine bestimmte Zeitspanne zu einem Datum hinzuzufügen oder davon zu subtrahieren, kann man auch die Operatoren `+` und `-` verwenden.

Es ist wichtig zu beachten, dass die `DateTime`-Klasse in C# immutable ist, das heißt, dass sie nicht verändert werden kann. Wenn man zum Beispiel ein Datum um 5 Tage erhöhen möchte, wird ein neues `DateTime`-Objekt zurückgegeben, anstatt das vorhandene zu ändern. Deshalb muss man das Ergebnis der `Add`-Methode oder der Operatoren einem neuen Objekt zuweisen.

## Siehe auch
- [offizielle Dokumentation zur DateTime-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.datetime)
- [Verwendung von TimeSpan in C#](https://docs.microsoft.com/de-de/dotnet/api/system.timespan?view=net-5.0)