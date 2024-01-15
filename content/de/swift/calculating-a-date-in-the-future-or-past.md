---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit."
html_title:           "Swift: Berechnung eines Datums in der Zukunft oder Vergangenheit."
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann in vielen Anwendungsfällen nützlich sein, z.B. in der Finanzwelt, bei der Verwaltung von Terminen oder für Reiseplanungen. Mit Swift können wir diese Berechnungen einfach und schnell durchführen.

## Wie geht das?

Die Berechnung eines Datums in der Zukunft oder Vergangenheit erfordert die Verwendung der `Date` Klasse in Swift. Zunächst müssen wir ein `Date` Objekt für das aktuelle Datum erstellen:

```Swift
let currentDate = Date()
```

Dann können wir die `Calendar` Klasse verwenden, um ein `DateComponents` Objekt zu erstellen, das die gewünschte Anzahl an Jahren, Monaten, Wochen, Tagen usw. enthält, die wir auf das aktuelle Datum addieren oder davon subtrahieren möchten. Zum Beispiel können wir ein Datum in 2 Monaten berechnen:

```Swift
let calendar = Calendar.current
var dateComponent = DateComponents()
dateComponent.month = 2
let futureDate = calendar.date(byAdding: dateComponent, to: currentDate)
```

Das `futureDate` Objekt enthält das berechnete Datum, also zwei Monate in der Zukunft.

## Tiefergehende Informationen

Beim Berechnen eines Datums in der Zukunft oder Vergangenheit ist es wichtig zu beachten, dass es nicht immer eine genaue Anzahl von Tagen oder Monaten gibt. Zum Beispiel kann ein Monat 28, 29, 30 oder 31 Tage haben. Deshalb ist es ratsam, die `Calendar` Klasse zu verwenden, um sicherzustellen, dass das berechnete Datum tatsächlich existiert. In der obigen Beispielrechnung verwendet die `Calendar` Klasse beispielsweise den korrekten Monat und das korrekte Jahr, um das Datum zu berechnen.

Es ist auch wichtig, die verschiedenen verfügbaren Methoden der `Date` Klasse zu verstehen, um Datumsformate und Zeitzonen zu berücksichtigen. Weitere Informationen dazu finden Sie in der offiziellen Swift-Dokumentation.

## Siehe auch

- [Apple Dokumentation zur Date Klasse](https://developer.apple.com/documentation/foundation/date)
- [Apple Dokumentation zur Calendar Klasse](https://developer.apple.com/documentation/foundation/calendar)
- [Swift By Example: Working With Dates](https://swiftbysundell.com/basics/dates/)