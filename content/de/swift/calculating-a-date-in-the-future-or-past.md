---
title:                "Swift: Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Warum man ein Datum in der Zukunft oder Vergangenheit berechnen sollte

Das Berechnen von Datumswerten in der Vergangenheit oder Zukunft ist eine nützliche Fähigkeit für viele Programmieraufgaben. Zum Beispiel können wir damit Buchungsdaten für eine Reise oder spezifische Ereignisdaten hinzufügen.

Wie man ein Datum in der Zukunft oder Vergangenheit berechnet

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir die Klasse "Date" aus der Swift Standard Library verwenden. Diese Klasse enthält viele Funktionen, um mit Datumswerten zu arbeiten.

```Swift
import Foundation // Importiere die Standard Library für Date

// Verwende "+" um Tage zu addieren
let zukunft = Date() + 7 // gibt das Datum in 7 Tagen aus
print(zukunft) // Beispieloutput: 2021-07-09 15:00:00 +0000

// Verwende "-" um Tage zu subtrahieren
let vergangenheit = Date() - 5 // gibt das Datum vor 5 Tagen aus
print(vergangenheit) // Beispieloutput: 2021-06-27 15:00:00 +0000

// Benutze die DateComponents Klasse, um ein spezifisches Datum zu berechnen
var datumsKomponenten = DateComponents() // Initialisiere die Klasse
datumsKomponenten.month = 8 // Setze den Monat auf August
datumsKomponenten.day = 15 // Setze den Tag auf 15
datumsKomponenten.year = 2021 // Setze das Jahr auf 2021
let benutzerdefiniertesDatum = Calendar.current.date(from: datumsKomponenten) // Verwende die Calendar Klasse, um ein Datum aus den Komponenten zu erstellen
print(benutzerdefiniertesDatum) // Beispieloutput: 2021-08-15 15:00:00 +0000
```

Tiefergehende Infos zur Berechnung von Datumswerten

Swift verwendet intern das gregorianische Kalendersystem, um Datumswerte zu berechnen. Die Funktionen der Date Klasse ermöglichen eine einfache Handhabung von Datumsberechnungen. Es ist außerdem möglich, Daten in verschiedenen Zeitzonen zu berechnen und zu konvertieren.

Eines ist jedoch wichtig zu beachten: Datumswerte in Swift sind immer in UTC-Zeitzone, was bedeutet, dass sie 0 Stunden voraus sind. Wenn man beabsichtigt, mit lokalen Zeitzonen zu arbeiten, muss man diese beim Berechnen von Datumswerten berücksichtigen oder die Zeitzon