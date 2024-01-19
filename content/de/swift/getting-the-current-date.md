---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums in Swift ermöglicht die Verfolgung von Zeitstempel in Programmen. Programmierer nutzen das für verschiedene Zwecke – von einfachen Prozessen wie Zeitanzeige über Zeitmessungen bis hin zu komplexeren Abläufen wie Prüfung von Gültigkeitsdaten Zertifikaten.

## Wie man es macht:

Hier ist ein kurzes Beispiel, wie man das aktuelle Datum in Swift erhält:

```Swift
import Foundation

let jetzt = Date()
print(jetzt)
```

Wenn Sie diesen Code ausführen, zeigt er das aktuelle Datum und die aktuelle Uhrzeit an, zum Beispiel:

```Swift
2021-10-08 11:45:20 +0000
```

## Tiefenanalyse:

### Historischer Kontext:

In frühen Versionen von Swift war der Umgang mit Datum und Zeit komplex und uneinheitlich. Mit Swift 3.0 wurde die `Date`-Klasse eingeführt, die das Arbeiten mit Daten erheblich vereinfacht hat.

### Alternativen:

Für speziellere Anforderungen kann man auf die `Calendar`-Klasse zurückgreifen. Mit `Calendar.current` kann man Details wie Jahr, Monat, Tag, Stunde, Minute usw. abrufen.

```Swift
let aktuellesDatum = Date()
let kalender = Calendar.current
let tag = kalender.component(.day, from: aktuellesDatum)
```

### Implementierungsdetails:

Beim Abrufen des aktuellen Datums in Swift wird ein `Date`-Objekt erstellt, das auf den aktuellen Zeitpunkt abgestimmt ist. Das `Date`-Objekt speichert Datum und Uhrzeit als einen einzelnen Wert relativ zum "Referenzdatum" (dem 1. Januar 2001, 00:00 Uhr UTC).

## Siehe auch:

- Apple Developer-Dokumentation: [Date](https://developer.apple.com/documentation/foundation/date) 
- Tutorial: [Arbeiten mit Datum und Zeit in Swift](https://www.hackingwithswift.com/read/1/3/dates-and-times-nsdate)