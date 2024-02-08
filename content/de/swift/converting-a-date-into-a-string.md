---
title:                "Datum in einen String umwandeln"
aliases:
- de/swift/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:34.912098-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln eines Datums in einen String erleichtert die Anzeige und Verarbeitung von Datumsangaben in einem für Menschen lesbaren Format. Wir brauchen diese Umwandlung für Benutzeroberflächen, Datenbanken und Logs.

## How to:
Swift bietet `DateFormatter` für das Konvertieren von `Date`-Objekten in Strings. Hier ist ein einfaches Beispiel:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy HH:mm"
let dateString = dateFormatter.string(from: Date())

print(dateString) // "Beispiel: 14.03.2023 19:45"
```

Mit `dateFormat` passt man das Format an den eigenen Bedarf an.

## Deep Dive:
Früher war das Umwandeln von Daten in Strings ein mühsamer Prozess, der fehleranfällig war. Heute vereinfacht `DateFormatter` in Swift diese Aufgabe. Trotzdem sollte man auf Performance achten, da die Erstellung von `DateFormatter`-Instanzen ressourcenintensiv sein kann. iOS 10 und macOS Sierra haben `ISO8601DateFormatter` eingeführt, einen spezialisierten Formatter für ISO 8601 Daten.

Alternativ können wir auch `DateComponentsFormatter` für relative Zeitangaben verwenden, z.B. "vor 5 Minuten". Für verschiedene Lokalisierungen sorgt `dateFormatter.locale = Locale(identifier: "de_DE")`. Das Stellen des `locale` ist wichtig, um die Sprache und regionale Formate korrekt zu handhaben.

Im Swift-Paket-Manager gibt es auch Drittanbieter-Libraries wie `SwiftDate`, die noch mehr Flexibilität bei der Datumsverarbeitung bieten.

## See Also:
- [Date Formatting Guide - Apple Developer](https://developer.apple.com/documentation/foundation/dateformatter)
- [SwiftDate Library auf GitHub](https://github.com/malcommac/SwiftDate)
