---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:11.260701-07:00
description: "Das aktuelle Datum in Swift zu bekommen, erfordert die Verwendung der\
  \ `Date`-Klasse, um auf das Datum und die Uhrzeit zuzugreifen, zu denen die App\u2026"
lastmod: '2024-03-13T22:44:54.235539-06:00'
model: gpt-4-0125-preview
summary: "Das aktuelle Datum in Swift zu bekommen, erfordert die Verwendung der `Date`-Klasse,\
  \ um auf das Datum und die Uhrzeit zuzugreifen, zu denen die App\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?
Das aktuelle Datum in Swift zu bekommen, erfordert die Verwendung der `Date`-Klasse, um auf das Datum und die Uhrzeit zuzugreifen, zu denen die App ausgeführt wird. Programmierer müssen aus einer Vielzahl von Gründen das aktuelle Datum abrufen, die von der Zeitstempelung von Ereignissen, der Durchführung von Datumsberechnungen bis hin zur Anzeige von Daten und Uhrzeiten in einer Benutzeroberfläche reichen.

## Wie geht das:
Das `Foundation`-Framework von Swift stellt die `Date`-Klasse bereit, was es unkompliziert macht, das aktuelle Datum und die Uhrzeit zu erhalten. Hier ist ein einfaches Beispiel dafür, wie man das aktuelle Datum bekommt:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Das wird etwas wie folgendes ausgeben:

```
2023-04-12 07:46:23 +0000
```

Das Ausgabeformat folgt dem ISO-8601-Standard unter Verwendung der UTC-Zeitzone. Sie möchten jedoch dieses Datum möglicherweise für Anzeigezwecke formatieren. Die `DateFormatter`-Klasse von Swift kommt zur Rettung:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Ein Beispiel für eine Ausgabe könnte sein:

```
12. April 2023 um 10:46:23
```

Beachten Sie, dass das Ausgabeformat je nach Locale des Geräts, das den Code ausführt, variieren wird.

Für Projekte, die komplexere Datumsmanipulationen erfordern, wenden sich viele Swift-Entwickler an Drittanbieter-Bibliotheken wie `SwiftDate`. So könnten Sie `SwiftDate` verwenden, um das aktuelle Datum in einer bestimmten Zeitzone und in einem bestimmten Format zu erhalten:

Zuerst fügen Sie `SwiftDate` Ihrem Projekt mit SPM, CocoaPods oder Carthage hinzu. Dann:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Das könnte ausgeben:

```
2023-04-12 09:46:23
```

Mit `SwiftDate` können Sie Daten und Zeiten für verschiedene Zeitzonen und Locales leicht manipulieren und damit komplexe Datumsbehandlungsaufgaben in Ihren Swift-Anwendungen vereinfachen.
