---
title:                "Swift: Die Umwandlung eines Datums in einen String"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung von Datumsangaben in Strings kann eine sehr nützliche Fähigkeit beim Programmieren sein. Sie ermöglicht es, die angezeigten Datumsangaben anzupassen und zu formatieren, um den spezifischen Anforderungen einer Anwendung gerecht zu werden.

## Wie es geht

Um ein Datum in einen String umzuwandeln, müssen wir zuerst ein `DateFormatter`-Objekt erstellen und es entsprechend konfigurieren. Dieser Formatter wird dann verwendet, um das Datum in einen String zu konvertieren.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
let date = Date()
let dateString = dateFormatter.string(from: date)
print(dateString) 
// Output: 15.08.2021
```

In diesem Beispiel haben wir den `dateFormat` des Formatters auf "dd.MM.yyyy" gesetzt, was bedeutet, dass das Datum im Format Tag.Monat.Jahr ausgegeben wird. Das Datum, das wir konvertieren, ist das aktuelle Datum, das durch die `Date()`-Funktion erhalten wird. Diese spezifische Konfiguration kann jedoch je nach Anforderungen individuell angepasst werden.

## Tiefere Einblicke

Das `DateFormatter`-Objekt bietet viele weitere Konfigurationsmöglichkeiten, die es ermöglichen, die ausgegebene Datumsangabe noch weiter anzupassen. Zum Beispiel können wir auch die Lokalisierung ändern, um das Datum in einer anderen Sprache oder einem anderen Format auszugeben.

```Swift
dateFormatter.locale = Locale(identifier: "de_DE")
let germanDateString = dateFormatter.string(from: date)
print(germanDateString)
// Output: 15.08.2021

dateFormatter.dateFormat = "MMM dd, yyyy"
let otherDateString = dateFormatter.string(from: date)
print(otherDateString)
// Output: Aug 15, 2021
```

Hier haben wir die Lokalisierung auf "de_DE" geändert, was die Datumsangabe in deutscher Sprache ausgibt. Wir haben auch das Datumsformat auf "MMM dd, yyyy" geändert, was das Datum im Monatsnamen, gefolgt vom Tag und Jahr ausgibt. Durch die Kombination verschiedener Einstellungen können wir präzise Kontrolle über das Ausgabeformat haben.

## Siehe auch

- [Apple Developer Documentation: Date Formatting Guide](https://developer.apple.com/documentation/foundation/date_formatter/date_formatting_guide)
- [Hacking with Swift: How to convert dates and times to a string using DateFormatter](https://www.hackingwithswift.com/articles/124/how-to-convert-dates-and-times-to-a-string-using-dateformatter)