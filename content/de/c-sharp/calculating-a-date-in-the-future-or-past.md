---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "C#: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Datum-Rechnung ist die Methode, um einen zukünftigen oder vergangenen Termin zu finden. Entwickler tun das oft, um anstehende Ereignisse zu planen oder vergangene Aktivitäten zu überprüfen.

## Wie geht das:

Verwenden Sie in C# die `AddDays`, `AddMonths` oder `AddYears` Funktionen. Hier sind einige Beispiele:

```CSharp
DateTime today = DateTime.Now;

//Add 7 days to current date
DateTime futureDate = today.AddDays(7);
Console.WriteLine("Future date: " + futureDate);

//Subtract 7 days from current date
DateTime pastDate = today.AddDays(-7);
Console.WriteLine("Past date: " + pastDate);
```

Ausgabe:

```CSharp
Future date: DD-MM-YYYY (aktuelles Datum + 7 Tage)
Past date: DD-MM-YYYY (aktuelles Datum - 7 Tage)
```

Bitte ersetzen Sie `DD-MM-YYYY` durch das aktuelle Datum.

## Vertiefung

Historisch gesehen haben Programmierer verschiedene Methoden verwendet, um das Datum zu berechnen, z.B. Julianisches Datum oder Unix-Zeit. Sie können auch Bibliotheken wie NodaTime verwenden, wenn Sie eine komplexere Datum- / Zeitberechnung benötigen.

Alternative Methoden, um das Datum zu berechnen:

```CSharp
//Using TimeSpan
DateTime now = DateTime.Now;
TimeSpan sevenDays = TimeSpan.FromDays(7);
DateTime futureDate = now + sevenDays;
```

## Siehe auch

Für weitere Informationen über DateTime und seine Methoden, besuchen Sie bitte diese Seiten:

- 'AddDays', 'AddMonths', und 'AddYears' Funktionen: 
  https://docs.microsoft.com/en-us/dotnet/api/system.datetime.adddays?view=net-5.0
  
- NodaTime Bibliothek: https://nodatime.org/

Bitte beachten Sie, dass Sie 'AddDays' verwenden können, um Tage, Wochen, Monate und Jahre hinzuzufügen oder zu subtrahieren. Für Monate und Jahre ändert sich die Anzahl der Tage je nachdem, welcher Monat oder welches Jahr es ist, so dass die Verwendung von 'AddMonths' oder 'AddYears' genauer sein kann.