---
title:                "Das aktuelle Datum abfragen"
html_title:           "Swift: Das aktuelle Datum abfragen"
simple_title:         "Das aktuelle Datum abfragen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das aktuelle Datum zu erhalten bedeutet, das heutige Datum in einer Variablen zu speichern. Programmierer tun dies, um in ihren Programmen auf das Datum zugreifen und es nutzen zu können.

## Wie geht's?

Verwende die ```Date()``` Funktion, um das aktuelle Datum zu erhalten und es einer Variablen zuzuweisen. Hier ist ein Beispielcode:

```Swift
let currentDate = Date()
```

Um das Datum in einem bestimmten Format auszugeben, verwende die ```DateFormatter``` Klasse. Hier ist ein Beispielcode, um das Datum im Format "dd.MM.yyyy" auszugeben:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let formattedDate = formatter.string(from: currentDate)
```

Die Ausgabe dieses Codes wäre beispielsweise "13.08.2021".

## Tiefere Einblicke

Die Notwendigkeit, das aktuelle Datum in Programmen zu haben, ergibt sich aus der häufigen Nutzung von Zeitstempeln in der Programmierung. Alternativ können Programmierer auch die ```Calendar``` oder ```NSCalendar``` Klasse verwenden, um das aktuelle Datum zu erhalten und zu formatieren. Diese beiden Klassen sind jedoch veraltet und sollten vermieden werden.

Eine interessante historische Tatsache ist, dass die Verwendung des Gregorianischen Kalenders in der Programmierung nicht immer Standard war. Frühere Programmiersprachen wie COBOL verwendeten beispielsweise den Julianischen Kalender.

## Siehe auch

Für weitere Informationen über die Verwendung von Datum und Zeit in Swift, schaue dir die offizielle Dokumentation an: [NSCalendar Class Reference](https://developer.apple.com/documentation/foundation/nscalendar)