---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "Swift: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Berechnen eines zukünftiges oder vergangenen Datums ist ein grundlegendes Arbeitspferd in der Programmierung - es bestimmt, wie viele Tage, Wochen, Monate oder Jahre in der Zukunft oder Vergangenheit von einem bestimmten Datum sind. Programmierer tun dies, um Zeiträume zu verfolgen, Ereignisse zu planen oder durch Zeitreihendaten zu navigieren.

## So geht's:
Im Swift können wir das `Calendar`-Objekt und seine Methode `date(byAdding:value:to:wrappingComponents:)` nutzen, um ein Datum im Zukunft oder Vergangenheit zu berechnen.

```Swift
// Aktuelles Datum holen
let currentDate = Date()

// Ein Kalender Objekt erstellen
let calendar = Calendar.current

// Fügen Sie 2 Jahre zum aktuellen Datum hinzu
if let newDate = calendar.date(byAdding: .year, value: 2, to: currentDate) {
    print(newDate)
}

//Subtrahieren Sie 3 Monate vom aktuellen Datum
if let oldDate = calendar.date(byAdding: .month, value: -3, to: currentDate) {
    print(oldDate)
}
```

## Tiefere Einblicke
Historisch gesehen haben Programmierer immer verschiedene Ansätze verfolgt, um ein zukünftiges oder vergangenes Datum zu berechnen. Früher, bevor Bibliotheken wie `Foundation` eingeführt wurden, mussten sie vielleicht manuell mit Tagen, Stunden, Minuten und Sekunden jonglieren. Mit der Einführung von `Calendar` in Swift ist diese Aufgabe jedoch viel einfacher und weniger fehleranfällig geworden.

Was die Alternativen betrifft, so könnten Entwickler auch andere Bibliotheken wie `DateComponents` oder Methoden wie `addingTimeInterval(_:)` in Erwägung ziehen. Während `Calendar`'s `date(byAdding:value:to:wrappingComponents:)` eine hohe Genauigkeit und Flexibilität bietet, kann die Verwendung von `addingTimeInterval(_:)` ausreichen, wenn man nur ein Datum und eine Zeit mit einem festen Intervall verschieben muss.

Die Implementierung der Datumskalkulation in Swift berücksichtigt auch verschiedene Kalender und Zeitzonen, was es zu einem mächtigen und universellen Werkzeug macht. Aber Vorsicht: das Berücksichtigen von Zeitzonen und Kalenderunterschieden kann zu unerwarteten Ergebnissen führen, wenn es nicht richtig gehandhabt wird.

## Siehe Auch
- Die Swift-Dokumentation für `Calendar`: https://developer.apple.com/documentation/foundation/calendar
- Die Swift-Dokumentation für `Date`: https://developer.apple.com/documentation/foundation/date
- Ein Artikel über Zeitzonen in Swift: https://www.hackingwithswift.com/articles/73/how-to-handle-different-time-zones-in-swift