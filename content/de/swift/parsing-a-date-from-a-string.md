---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String ist ein Prozess, bei dem ein Textformat in ein Datumsobjekt umgewandelt wird. Programmierer machen dies, um Daten in ein benutzerfreundlicheres und einfacher zu verarbeitendes Format zu bringen.

## So Geht's:

In Swift können wir das `DateFormatter`-Objekt verwenden, um Datumsstrings zu parsen. Hier ist ein einfaches Beispiel.

```Swift
import Foundation

let datumsString = "2020-12-31"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"

if let datum = formatter.date(from: datumsString) {
    print("Das Datum ist \(datum).")
} else {
    print("Ungültiges Datumsformat.")
}
```

Wenn wir diesen Code ausführen, ist das Ausgabeergebnis: 

```Swift
Das Datum ist 2020-12-31 00:00:00 +0000.
```
## Vertiefung:

(1) Historischer Kontext: Die Notwendigkeit, Datumswerte zu parsen, besteht bereits seit den frühen Tagen der Programmierung. Früher wurden oft eigene Funktionen dafür geschrieben, aber moderne Sprachen wie Swift bieten eingebaute Werkzeuge dafür.

(2) Alternativen: Verschiedene Programmiersprachen haben unterschiedliche Methoden zur Bearbeitung von Datumswerten. In Python zum Beispiel verwenden wir das `datetime` Modul, während in JavaScript die `Date`-Klasse verwendet wird.

(3) Implementierungsdetails: Der `DateFormatter` in Swift arbeitet nach den ICU-Spezifikationen, was bedeutet, dass er eine große Vielfalt an Datumsformaten unterstützt. Weiterhin ist zu beachten, dass das Parsen fehlschlägt, wenn das Datumsformat nicht mit dem des Strings übereinstimmt.

## Siehe Auch:

Für weitere Informationen zum Parsen von Datumswerten, schauen Sie sich die folgenden Quellen an:

- Apple's offizielle Dokumentation zu [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [International Components for Unicode (ICU) User Guide](https://unicode-org.github.io/icu/userguide/formatParse/messages/)
- Stack Overflow Diskussion zu [Date Parsing in Swift](https://stackoverflow.com/questions/35700281/date-format-in-swift) 

Erinnern Sie sich, Date Parsing ist ein alltägliches Problem in der Programmierung, und Swift macht es uns leicht, es zu lösen!