---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att jämföra två datum innebär att bestämma vilket av de två datumen som kommer först eller om de är samma datum. Detta är nödvändigt för att organisera händelser, sortera data och utföra beräkningar baserat på tid.

## Hur gör man:

Vi kommer att använda Swifts innebyggda funktioner för datumjämförelse: "compare(_:)" och "timeIntervalSince(_:)".

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"
let date1 = dateFormatter.date(from: "2022/03/31 14:00")
let date2 = dateFormatter.date(from: "2022/04/01 10:00")

// Använda compare(_:)
switch date1?.compare(date2!) {
case .orderedAscending:
    print("Datum 1 kommer före datum 2")
case .orderedSame:
    print("Datum 1 och datum 2 är samma")
case .orderedDescending:
    print("Datum 1 kommer efter datum 2")
case .none:
    print("Kunde inte jämföra datumen")
}

// Använda timeIntervalSince(_:)
if let timeInterval = date1?.timeIntervalSince(date2!) {
    if timeInterval < 0 {
        print("Datum 1 kommer före datum 2")
    } else if timeInterval == 0 {
        print("Datum 1 och datum 2 är samma")
    } else {
        print("Datum 1 kommer efter datum 2")
    }
}
```

## Djupdykning

Swifts datumhantering är baserad på Cocoa och NSDates historiska implementation. Alternativt kan du använda tredjepartsbibliotek som SwiftDate för mer anpassningsbara jämförelser och datumhantering. 

När Swift jämför två datum betraktas de som punkter längs en tidslinje snarare än kalenderdatum. Därför, när du jämför två datum i Swift, jämför den faktiskt tidpunkterna för de två datumen ner till bråkdelar av en sekund.

## Se också

- [Apple dokumentation Date](https://developer.apple.com/documentation/foundation/date)
- [SwiftDate på GitHub](https://github.com/malcommac/SwiftDate)
- [NSDate i Objective-C](https://developer.apple.com/documentation/foundation/nsdate)