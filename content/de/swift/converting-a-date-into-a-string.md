---
title:                "Eine Datum in einen String umwandeln."
html_title:           "Swift: Eine Datum in einen String umwandeln."
simple_title:         "Eine Datum in einen String umwandeln."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Konvertierung eines Datums in einen String ist ein häufiges Konzept in der Programmierung, das es ermöglicht, ein Datum in einem lesbaren Format anzuzeigen. Programmierer tun dies oft, um Informationen dem Benutzer zu präsentieren oder Daten aufzubereiten, um sie in einer Datenbank oder Datei zu speichern.

## Wie geht's:
Beispiel 1:
```Swift
let date = Date()

let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"

let dateString = formatter.string(from: date)
//Output: 26.07.2021
```

Beispiel 2:
```Swift
let date = Date()

let formatter = DateFormatter()
formatter.locale = Locale(identifier: "de_DE")
formatter.dateStyle = .medium
formatter.timeStyle = .short

let dateString = formatter.string(from: date)
//Output: 26. Juli 2021 um 10:30
```
In diesen Beispielen wird das aktuelle Datum mit verschiedenen Date Formatter-Einstellungen in einen String konvertiert. In Beispiel 1 wird das Datum im Format "Tag.Monat.Jahr" angezeigt, während in Beispiel 2 das Datum in einem formatierten Stil für die deutsche Sprache angezeigt wird.

## Tiefergehende Erläuterung:
Die Konvertierung eines Datums in einen String ist ein grundlegendes Konzept in der Programmierung und hat sich im Laufe der Zeit weiterentwickelt. Früher wurde dies oft durch die Verwendung von Funktionen wie "printf" in C oder "toString" in Java erreicht. Mit der Einführung von Swift wurde das Date Formatter-Objekt eingeführt, das eine einfachere Möglichkeit bietet, Datumswerte in Strings zu konvertieren.

Es gibt auch alternative Methoden, um Datumswerte in einem lesbaren Format anzuzeigen, wie zum Beispiel die Verwendung von Bibliotheken von Drittanbietern oder die Implementierung eigener Funktionen. Letztendlich hängt die Wahl davon ab, welche Anforderungen ein Programm hat und welche Methode die beste Option ist.

Bei der Konvertierung eines Datums in einen String ist es auch wichtig, die verschiedenen Datums- und Zeitzonenformate zu berücksichtigen. Date Formatter bietet die Möglichkeit, diese Einstellungen zu ändern, um sicherzustellen, dass das Datum korrekt dargestellt wird, unabhängig von der Einstellung des Geräts oder des Benutzers.

## Siehe auch:
- [Apple Dokumentation zum Date Formatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Tutorial zum Konvertieren von Daten in Strings in Swift](https://www.hackingwithswift.com/example-code/system/how-to-convert-dates-and-times-to-a-string-using-dateformatter)
- [Einführung in die Arbeit mit Datumswerten in Swift](https://www.raywenderlich.com/179848/swift-date-tutorial-getting-started)