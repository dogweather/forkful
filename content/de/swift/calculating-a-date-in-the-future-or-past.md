---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Swift: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Berechnen von zukünftigen oder vergangenen Daten ist eine häufige Aufgabe für Programmierer. Dabei wird ein bestimmtes Datum, basierend auf einer vorgegebenen Anzahl von Tagen, Monaten oder Jahren verändert. Programmierer tun dies, um beispielsweise Termine oder Ereignisse zu planen oder um die Funktion von Anwendungen zu erweitern.

# Wie geht's?

```Swift
// Beispiel, um das Datum 30 Tage in der Zukunft zu berechnen
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy" // Format des Datums angeben
if let currentDate = dateFormatter.date(from: "01.01.2020") { // aktuelles Datum festlegen
    let futureDate = Calendar.current.date(byAdding: .day, value: 30, to: currentDate) // Berechnung des Datums für 30 Tage in der Zukunft
    if let newDate = futureDate {
        print(dateFormatter.string(from: newDate)) // Ausgabe des berechneten Datums im angegebenen Format
    }
}

/* Ergebnis: 31.01.2020 */
```

# Tiefere Einblicke

(1) Die Berechnung von zukünftigen oder vergangenen Daten ist seit den Anfängen der Programmierung wichtig und wird in vielen verschiedenen Anwendungsbereichen eingesetzt.

(2) Alternativ zu Swift gibt es weitere Programmiersprachen, die ebenfalls Methoden zur Berechnung von Datumsänderungen anbieten, wie z.B. JavaScript mit der Date Object Methode.

(3) Die Berechnung von Datumsänderungen erfordert ein Verständnis von Datum formatspezifischen Funktionen und Methoden, sowie die Verwendung von Kalenderdaten.

# Siehe auch

- Apple Developer Dokumentation: https://developer.apple.com/documentation/foundation/datecomponents
- Tutorial zur Berechnung von Datumsänderungen in Swift: https://www.hackingwithswift.com/articles/113/how-to-calculate-a-relative-date-such-as-one-week-ago-using-dates-components