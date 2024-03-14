---
date: 2024-01-20 17:37:37.962413-07:00
description: "Konvertering av dato til streng gj\xF8r datoen menneskeleselig. Programmerere\
  \ trenger dette for \xE5 vise datoer i apper og logger som folk faktisk forst\xE5\
  r."
lastmod: '2024-03-13T22:44:41.153581-06:00'
model: gpt-4-1106-preview
summary: "Konvertering av dato til streng gj\xF8r datoen menneskeleselig. Programmerere\
  \ trenger dette for \xE5 vise datoer i apper og logger som folk faktisk forst\xE5\
  r."
title: Konvertere en dato til en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av dato til streng gjør datoen menneskeleselig. Programmerere trenger dette for å vise datoer i apper og logger som folk faktisk forstår.

## Hvordan:
I Swift bruker vi `DateFormatter` for å gjøre om datoer til streng. Her er et eksempel:

```Swift
import Foundation

let nå = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy HH:mm"
let datoStreng = formatter.string(from: nå)
print(datoStreng)
```

Om du kjører koden, vil resultatet se slik ut (dato og klokkeslett vil variere):

```
// Output
29.03.2023 14:37
```

## Dypdykk:
Før Swift og iOS, brukte Objective-C `NSDateFormatter` - samme konsept, annen innpakning. Å konvertere dato til streng er viktig for lokalisering; brukere i forskjellige land leser datoer ulikt. 

Alternativer inkluderer å bruke `ISO8601DateFormatter` for mer standardiserte formater, eller til og med `.description` på `Date`-objekter for en rask, men mindre kontrollerbar, løsning. 

Implementeringsdetaljer involverer ofte time-sone justeringer og å ta hensyn til brukernes lokasjon for å vise datoer korrekt. `DateFormatter` har egenskaper som `timeZone` og `locale` for å håndtere dette.

## Se Også:
- [Date Formatting Guide](https://developer.apple.com/documentation/foundation/dateformatter) fra Apple
- [Swift’s Date and Time API](https://nshipster.com/datecomponents/) på NSHipster for en grundig gjennomgang
