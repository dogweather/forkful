---
title:                "C#: Eine Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Eine Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Datum kann in vielen verschiedenen Programmierprojekten nützlich sein. Zum Beispiel kann es hilfreich sein, um Termine in Kalenderanwendungen oder zur Berechnung von Fristen in Geschäftsanwendungen zu verwenden. Mit Hilfe von C# können Sie ganz einfach solche Berechnungen durchführen und Ihr Programm benutzerfreundlicher und effizienter gestalten.

## Wie geht das?

Das Berechnen von zukünftigen oder vergangenen Daten in C# ist relativ einfach. Zunächst müssen Sie mithilfe der DateTime-Klasse eine Instanz eines Datumsobjekts erstellen. Diese Klasse enthält eine Vielzahl von Methoden und Eigenschaften, die Sie bei der Berechnung von Datumswerten unterstützen.

```C#
DateTime now = DateTime.Now; //aktuelles Datum

//Zukünftiges Datum berechnen
DateTime futureDate = now.AddDays(30); //fügt 30 Tage zum aktuellen Datum hinzu

//Vergangenes Datum berechnen
DateTime pastDate = now.AddYears(-2); //zieht 2 Jahre vom aktuellen Datum ab

//Datum formatieren
Console.WriteLine("Das zukünftige Datum ist: " + futureDate.ToString("dd.MM.yyyy"));
Console.WriteLine("Das vergangene Datum ist: " + pastDate.ToString("dd.MM.yyyy"));

/* Output:
Das zukünftige Datum ist: 18.10.2021
Das vergangene Datum ist: 17.10.2019 
*/
```

## Tiefere Einblicke

Beim Berechnen von Daten ist es wichtig, die Datentypen genau zu kennen. In C# gibt es verschiedene Datentypen, die zur Darstellung von Datums- und Zeitwerten verwendet werden können. Die bekanntesten sind DateTime, DateTimeOffset und TimeSpan. Es ist wichtig, die Unterschiede zwischen diesen Typen zu verstehen, um die richtige Methode für Ihre Berechnungen auszuwählen.

Es ist auch möglich, Datumswerte in verschiedenen Zeitzonen zu berechnen. C# bietet die Klasse TimeZoneInfo, die es Ihnen ermöglicht, Datumswerte in verschiedene Zeitzonen zu konvertieren. Dies ist besonders nützlich, wenn Sie mit internationalen Datumsangaben arbeiten.

## Siehe auch

- [DateTime-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.datetime?view=netcore-3.1)
- [DateTimeOffset-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.datetimeoffset?view=netcore-3.1)
- [Zeitzone-Unterstützung in C#](https://docs.microsoft.com/de-de/dotnet/standard/datetime/time-zones)