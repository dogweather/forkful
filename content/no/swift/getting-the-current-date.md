---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:16:46.998458-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I programmering henter vi gjeldende dato for å merke hendelser, sjekke varighet, eller når vi trenger nåværende tidspunkt for funksjoner som dato- og tidsstempler.

## Hvordan:
```Swift
import Foundation

let nå = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let datoStreng = formatter.string(from: nå)
print(datoStreng)
```
Eksempel på utskrift: `2023-04-05 14:23:07`

## Dykk Dypt:
Før iPhone og Swift var Objective-C og NS-klasser Apple sin gå-til for datum og tid håndtering. Nå foretrekker vi `Date` og `DateFormatter` for sin enkelhet og fleksibilitet. Alternativer som `Calendar` og `DateComponents` gir mer kontroll for komplekse operasjoner. Viktig detalj: `Date()` gir tid i UTC, så bruk `TimeZone` om du trenger lokal tid.

## Se Også:
- [Swift Documentation for DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift API for Calendar and DateComponents](https://developer.apple.com/documentation/foundation/calendar)