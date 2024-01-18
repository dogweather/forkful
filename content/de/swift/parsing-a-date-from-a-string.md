---
title:                "Eine Datum von einem String analysieren"
html_title:           "Swift: Eine Datum von einem String analysieren"
simple_title:         "Eine Datum von einem String analysieren"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist ein häufiges Problem, dem Programmierer in Swift gegenüberstehen. Dabei geht es darum, aus einem Text eine als Datum formatierte Information auszulesen und in einem geeigneten Datentypen zu speichern. Oft muss dieser Vorgang automatisch durchgeführt werden, um Daten aus externen Quellen zu verarbeiten.

## Wie man es macht:

Die einfachste Möglichkeit, ein Datum aus einem String zu parsen, ist die Verwendung der Date-Initialisierungsfunktion mit einem Date Formatter. Dabei muss der String im richtigen Datumsformat vorliegen, damit das Initialisieren erfolgreich ist. Ein Beispiel:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let dateString = "15.07.2020"
if let date = dateFormatter.date(from: dateString) {
    print(date)
}
```

Dieses Beispiel zeigt das Parsen eines Datums aus einem String im Format "15.07.2020". Nach der Initialisierung des Date Formatters mit dem entsprechenden Datumsformat wird der String mithilfe der `date(from:)` Funktion in ein Date Objekt umgewandelt und ausgegeben.

Eine weitere Möglichkeit ist die Verwendung der `ISO8601DateFormatter`, um Datumsangaben im internationalen Standardformat zu parsen. Ein Beispiel:

```Swift
let isoFormatter = ISO8601DateFormatter()
let dateString = "2020-07-15T12:00:00Z"
if let date = isoFormatter.date(from: dateString) {
    print(date)
}
```

Dieses Beispiel zeigt das Parsen eines Datums aus einem String im ISO8601 Format. Auch hier wird der Formatter verwendet, um das Datum zu parsen und in ein Date Objekt umzuwandeln und auszugeben.

## Tiefgehende Analyse:

Das Parsen von Daten aus einem String hat eine lange Geschichte und war in früheren Versionen von Swift nicht so einfach zu bewerkstelligen. Bevor es die `DateFormatter` Klasse gab, mussten die Datumsformate manuell mit einer Kombination aus `substring` und `range(of:)` Funktionen aufgeteilt werden. Diese Lösung war jedoch fehleranfällig und ungenau.

Alternativ zum `DateFormatter` gibt es auch die Möglichkeit, Datumsangaben in `String` Objekte umzuwandeln und mithilfe von Regulären Ausdrücken zu parsen. Dies ist jedoch zeitaufwändiger und erfordert umfangreichere Kenntnisse in Bezug auf Regular Expressions.

Die Implementierung des `DateFormatter` ist auf maximale Genauigkeit ausgelegt, was bedeutet, dass er die kalendarischen Besonderheiten verschiedener Länder und Regionen berücksichtigt und so exakte Datumsangaben liefern kann.

## Siehe auch:

Für weitere Informationen und Beispiele zum Parsen von Daten aus Strings in Swift empfehle ich die offizielle Dokumentation von Apple sowie diese informative Anleitung von Hacking with Swift: 
- [Apple Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Parsing Dates from Strings with DateFormatter in Swift](https://www.hackingwithswift.com/example-code/system/how-to-parse-dates-from-strings-using-dateformatter)