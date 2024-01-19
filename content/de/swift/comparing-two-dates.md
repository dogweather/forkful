---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vergleich von zwei Daten mit Swift: Ein Leitfaden

## Was & Warum?

Das Vergleichen zweier Daten bedeutet, zu prüfen, ob ein Datum vor, gleich oder nach einem anderen Datum ist. Dies ist häufig notwendig für Logik rund um Termine, Ereignisse und Zeitverfolgung in vielen Arten von Software.

## Wie es geht:

Hier ist ein einfaches Beispiel dafür, wie Sie zwei Daten in Swift vergleichen können:

```Swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy/MM/dd"
let datum1 = formatter.date(from: "2022/05/01")
let datum2 = formatter.date(from: "2022/06/01")

if datum1! < datum2! {
    print("Datum1 ist vor Datum2")
} else {
    print("Datum1 ist nicht vor Datum2")
}
```

Wenn Sie dieses Code ausführen, gibt es "Datum1 ist vor Datum2" aus.

## Tieferes Eintauchen

Historisch gesehen verwendet Swift `Date` zum Vergleichen von Daten und `DateComponents` zum Manipulieren von Daten. Für eine genaue Zeitmessung sollten Sie das `DateInterval` Modul verwenden.

Obwohl die `Date`-Klasse in Swift recht gut ist, gibt es Alternativen wie "SwiftDate" und "Timepiece". Diese Bibliotheken haben zusätzliche Funktionen und können einfacher zu verwenden sein, basierend auf Ihren spezifischen Anforderungen.

Bedenken Sie, dass das Ergebnis des Datenvergleichs vom Zeitzonenkontext abhängt. Stellen Sie sicher, dass Sie Ihre Daten korrekt und mit den richtigen Zeitangaben vergleichen.

## Siehe auch:

- [Apple's Date Class Reference](https://developer.apple.com/documentation/foundation/date)
- [How to Compare Dates With Swift](https://www.hackingwithswift.com/example-code/system/how-to-compare-dates) 
- [DateFormater Class Reference](https://developer.apple.com/documentation/foundation/dateformatter) 
- [SwiftDate](https://github.com/malcommac/SwiftDate) 
- [Timepiece](https://github.com/naoty/Timepiece)