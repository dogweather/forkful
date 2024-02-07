---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
date:                  2024-01-20T17:31:53.492713-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist das Ermitteln eines spezifischen Zeitpunkts vor oder nach einem bekannten Datum. Programmierer tun dies häufig für Features wie Erinnerungen, Buchungs-Systeme oder Ablaufdaten.

## How to:

Swift bietet das `Date` und `Calendar` Framework für Datumsberechnungen. Hier ein paar Beispiele:

```Swift
import Foundation

// Heutiges Datum erhalten
let heute = Date()

// Kalender instanziieren
var kalender = Calendar.current

// 5 Tage in die Zukunft reisen
if let zukunft = kalender.date(byAdding: .day, value: 5, to: heute) {
    print("In 5 Tagen ist es \(zukunft).")
}

// 3 Wochen in die Vergangenheit gehen
if let vergangenheit = kalender.date(byAdding: .weekOfYear, value: -3, to: heute) {
    print("Vor 3 Wochen war es \(vergangenheit).")
}

// Ausgabe
// In 5 Tagen ist es <Zukünftiges Datum>.
// Vor 3 Wochen war es <Vergangenes Datum>.
```

## Deep Dive

Berechnungen mit Daten sind nich neu. Historisch wurden Kalendersysteme angepasst, um präzisere Zeitberechnungen zu erlauben – denken wir an die Gregorianische Kalenderreform. In der Programmierung haben wir es einfacher. Statt astronomische Tabellen zu wälzen, nutzen wir Frameworks. `DateComponents` bietet eine Alternative zu `date(byAdding:value:to:)`. Es lässt uns spezifischere Anforderungen stellen:

```Swift
let komponenten = DateComponents(year: 1, month: 2, day: 3)
if let zukunftigesDatum = kalender.date(byAdding: komponenten, to: heute) {
    print("In einem Jahr, zwei Monaten und drei Tagen ist es \(zukunftigesDatum).")
}
```

Beim Implementieren solltest du Zeitzonen und lokale Kalendereinstellungen beachten. `Calendar.current` bezieht sich auf die Systemeinstellungen des Users.

## See Also

- `DateComponents` und `Calendar` Dokumentation: [Date and Time Programming Guide for Cocoa](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
