---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Swift: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Datumberäkning handlar om att räkna fram eller tillbaka i tiden från ett specifikt datum. Programmare gör detta för att hantera tidsfrist-relaterade uppgifter såsom påminnelser, händelser och schemaläggningar.

## Så här gör du:

Swift erbjuder förenklat arbete med datum genom `DateComponents` och `Calendar`. Här är ett exempel på att addera 5 dagar till dagens datum:

```Swift
import Foundation

let nu = Date()
let kalender = Calendar.current

if let omFemDagar = kalender.date(byAdding: .day, value: 5, to: nu) {
    print(omFemDagar)
}
```

Denna kod ger oss datumet just nu, plus fem dagar. Output kommer se ut något i stil med `2023-11-29 09:29:29 +0000`.

## Djupdykning:

Historiskt sett, att räkna ut tid har alltid varit ett kritiskt ämne inom datorsystem. Tidigare metoder involverade komplicerade algoritmer för att kunna hantera skottår och tidszoner. Med Swift har dessa processer blivit mycket enklare.

Ett alternativ för Swift-programmerare är att använda `DateInterval` och `DateComponentsFormatter` för att hantera datumrelaterade beräkningar. Dessutom kan tredjepartsbibliotek som SwiftDate erbjuda mer komplex funktionalitet.

Swifts lösning på datumhantering är baserad på `NSCalendar` och `NSDateComponents` som finns i Foundation-ramverket. Dessa tillhandahåller en mängd metoder för att manipulera och jämföra datum.

## Se även:

Apple Developer Documentation: [DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)

Apple Developer Documentation: [Calendar](https://developer.apple.com/documentation/foundation/calendar)

Andra bibliotek: [SwiftDate](https://github.com/malcommac/SwiftDate)