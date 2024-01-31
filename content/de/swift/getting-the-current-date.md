---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:16:39.669551-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist das Lesen des momentanen Datums und der Uhrzeit des Systems. Programmierer nutzen dies für Funktionen wie Zeitstempel, Zeitplanung von Aufgaben oder Gültigkeitsdauern von Sessions.

## So geht's:
Hier ist ein einfaches Beispiel, wie man das aktuelle Datum in Swift erhält:

```Swift
import Foundation

let aktuellesDatum = Date()
print(aktuellesDatum)
```

Ausgabemuster könnte so aussehen:

```Swift
2023-04-05 14:23:50 +0000
```

## Deep Dive:
Das `Date`-Objekt in Swift stellt einen bestimmten Punkt in der Zeit dar. Hierbei handelt es sich um eine Momentaufnahme der aktuellen Systemuhr. Historisch gesehen hat Apple API-Veränderungen durchgeführt, um Zeit und Datum besser zu handhaben, z.B. von `NSDate` in Objective-C zu `Date` in Swift. Alternative Methoden, um das Datum zu erhalten, beinhalten Verwendung von Kalendern für spezifische Zeitberechnungen oder Timezones, um das Datum in verschiedenen Zeitzonen zu repräsentieren. Die Implementierung hinter `Date` verwendet das Coordinated Universal Time (UTC) Format, garantiert also eine hohe Genauigkeit und verhindert Probleme bei der Umstellung von Sommer- auf Winterzeit und umgekehrt.

## Siehe auch:
- Swift-Dokumentation zu `Date`: [Swift Date](https://developer.apple.com/documentation/Foundation/Date)
- Informationen über Zeitzone in Swift: [Time Zone](https://developer.apple.com/documentation/Foundation/TimeZone)
