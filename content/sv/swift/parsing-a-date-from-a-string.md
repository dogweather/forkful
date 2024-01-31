---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:38:40.639319-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka (parse) ett datum från en sträng innebär att omvandla text till ett datumobjekt. Programmerare gör detta för att hantera datumdata effektivt, exempelvis för att jämföra datum eller beräkna tidsintervaller.

## Så här gör du:
Swift använder `DateFormatter` för att tolka datum. Sätt format, skapa datum och använd det.

```Swift
import Foundation

let dateString = "2023-04-02"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
if let date = dateFormatter.date(from: dateString) {
    print("Datumet är \(date)")
} else {
    print("Kunde inte tolka datumet.")
}
```

Output:
```
Datumet är 2023-04-01 22:00:00 +0000
```

Observera tidszonen (`+0000`) i output. 

## Fördjupning
Datumtolkning startade före Swift, med `NSDateFormatter` i Objective-C. Swift förenklade processen med `DateFormatter`.

Alternativ:
- `ISO8601DateFormatter` för ISO 8601 datumformat.
- Tredjepartsbibliotek som `SwiftDate` för mer funktioner.

Implementation:
- `Locale` påverkar tolkning, som `dateFormatter.locale = Locale(identifier: "en_US_POSIX")`.
- Använd `dateFormatter.timeZone` för tidszoner.

## Se även
- Swift Documentation: [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- SwiftDate Bibliotek: [SwiftDate GitHub](https://github.com/malcommac/SwiftDate)
