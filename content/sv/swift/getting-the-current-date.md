---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:16:57.519172-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta aktuellt datum innebär att få tag på datum och tid just nu. Programmerare gör detta för att hantera deadlines, tidstämplar, schemaläggningar och mycket mer.

## Hur man gör:
```Swift
import Foundation

let nu = Date()
print(nu)
```
Exempel på utskrift:
```
2023-04-01 12:00:00 +0000
```

För finjustering och format:
```Swift
let formatter = DateFormatter()
formatter.dateStyle = .short
formatter.timeStyle = .long

let formatDatum = formatter.string(from: nu)
print(formatDatum)
```
Exempel på utskrift:
```
01/04/2023, 12:00:00 GMT+1
```

## Djupdykning:
Historiskt sett har hantering av datum och tid varit knepigt. Tidszoner, skottsekunder och olika kalendrar komplicerar. I Swift hanteras detta genom `Date`-klassen i Foundation-biblioteket. 

Ett alternativ till `Date()` är att använda `DateComponents` för mer kontroll:
```Swift
var komponenter = DateComponents()
komponenter.year = 2023
komponenter.month = 4
komponenter.day = 1
komponenter.timeZone = TimeZone(abbreviation: "CET") // Centraleuropeisk tid
komponenter.hour = 12
komponenter.minute = 0

let kalender = Calendar.current
if let specifiktDatum = kalender.date(from: komponenter) {
    print(specifiktDatum)
}
```

När det gäller prestanda är `Date()` snabb och effektiv för de flesta användningsfall. Det hanterar automatiskt tidzoner baserat på användarens enhetsinställningar.

## Se även:
- [TimeZone dokumentation på Apple Developer](https://developer.apple.com/documentation/foundation/timezone)
- [DateFormatter dokumentation på Apple Developer](https://developer.apple.com/documentation/foundation/dateformatter)