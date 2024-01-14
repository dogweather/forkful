---
title:                "Swift: Umwandeln eines Datums in einen String"
simple_title:         "Umwandeln eines Datums in einen String"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datum zu String ist eine häufige Aufgabe in der Swift Programmierung. Es ermöglicht uns, Daten in einem für den Benutzer leicht verständlichen Format darzustellen.

## Wie geht man vor?

Es gibt mehrere Möglichkeiten, ein Datum in einen String umzuwandeln. Hier sind zwei Beispiele, die die gängigsten Methoden zeigen:

```Swift
let date = Date() // Erstelle ein Datum Objekt
let formatter = DateFormatter() // Initialisiere den DateFormatter 
formatter.dateFormat = "dd.MM.yyyy" // Wähle das gewünschte Ausgabeformat
let stringDate = formatter.string(from: date) // Konvertiere das Datum in einen String

print(stringDate) // Output: 05.07.2021
```

In diesem Beispiel verwenden wir die `DateFormatter` Klasse, um das Datum in einen String zu konvertieren. Zuerst erstellen wir ein `Date` Objekt mit dem aktuellen Datum. Dann initialisieren wir einen `DateFormatter` und wählen das gewünschte Ausgabeformat aus, in diesem Fall "dd.MM.yyyy" für den Tag, Monat und Jahr. Schließlich verwenden wir die `string(from:)` Methode des DateFormatters, um das Datum in einen String zu konvertieren.

Eine andere Möglichkeit ist die Verwendung der `String` Initialisierungsmethode, die ein Datumformat als Parameter erwartet:

```Swift
let date = Date() // Erstelle ein Datum Objekt
let stringDate = String(describing: date, format: "yyyy-MM-dd") // Konvertiere das Datum in einen String im Format "yyyy-MM-dd"

print(stringDate) // Output: 2021-07-05
```

## Tiefer Einblick

Die `DateFormatter` Klasse bietet viele nützliche Methoden, um das Ausgabeformat für ein Datum anzupassen. Hier sind einige Beispiele:

- `localizedString(from: date)` wandelt ein Datum in einen lokalisierten String um, abhängig von der Regionseinstellung des Geräts.
- `dateFormat` gibt das aktuell verwendete Datumsformat als String zurück.
- `timeZone` ermöglicht es uns, die Zeitzone für die Ausgabe des Datums festzulegen.

Es ist auch wichtig zu beachten, dass das Format für den Tag, Monat und Jahr je nach Region variieren kann. Es ist daher ratsam, die `DateFormatter` Klasse zu verwenden, um sicherzustellen, dass das Datum korrekt im gewünschten Format ausgegeben wird.

## Siehe auch

- [Apple Developer Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift.org: Date and Time Formatting](https://swift.org/blog/date-and-time/#date-and-time-formatting)