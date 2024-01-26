---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:38:42.177660-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Datum aus einem String parsen bedeutet, geschriebenes Datum in ein Date-Objekt umzuwandeln. Das ist nötig, um mit Datumsangaben zu rechnen, sie zu vergleichen oder in einem anderen Format auszugeben.

## How to:

```Swift
import Foundation

// String zu Datum mit DateFormatter
let dateString = "31.03.2023"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy"
dateFormatter.locale = Locale(identifier: "de_DE")

if let date = dateFormatter.date(from: dateString) {
    print("Das geparste Datum ist: \(date)")
} else {
    print("Parsing des Datums fehlgeschlagen.")
}
```

Sample Output:

```
Das geparste Datum ist: 2023-03-30 22:00:00 +0000
```

## Deep Dive

Das Parsen von Datumsangaben aus Strings ist essenziell, da sehr viele menschliche Interaktionen mit Daten darüber laufen. DateFormatter ist Teil von Foundation seit Swift's Anfangstagen und bietet einen flexiblen Weg, Strings in Date-Objekte umzuwandeln. Alternativ gibt es Libraries wie `SwiftDate`, die noch mehr Funktionalitäten bereitstellen. Die Implementierung von DateFormatter beruht auf den Locale-Einstellungen, was bedeutet, dass ohne passende Locale, das Parsing scheitern kann – achte also darauf!

## See Also

- [ISO8601DateFormatter](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
- [SwiftDate GitHub Repository](https://github.com/malcommac/SwiftDate)
