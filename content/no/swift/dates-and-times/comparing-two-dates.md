---
date: 2024-01-20 17:34:06.450092-07:00
description: "How to: (Hvordan:) F\xF8r Swift og moderne programmeringsspr\xE5k, var\
  \ dato-manipulasjon tungvint og feilutsatt. Forskjeller i tidssoner og kalendersystemer\u2026"
lastmod: '2024-04-05T22:50:55.160496-06:00'
model: gpt-4-1106-preview
summary: "(Hvordan:) F\xF8r Swift og moderne programmeringsspr\xE5k, var dato-manipulasjon\
  \ tungvint og feilutsatt."
title: Sammenlikning av to datoer
weight: 27
---

## How to: (Hvordan:)
```Swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy/MM/dd HH:mm"

// Anta at vi har to strenger for datoer.
let dateString1 = "2023/04/01 14:00"
let dateString2 = "2023/04/02 10:00"

// Konverter strengene til Date objekter.
let date1 = formatter.date(from: dateString1)!
let date2 = formatter.date(from: dateString2)!

// Sammenlign datoene.
if date1 < date2 {
    print("date1 er før date2")
} else if date1 > date2 {
    print("date1 er etter date2")
} else {
    print("Datoene er like")
}
```
Sample Output:
```
date1 er før date2
```

## Deep Dive (Dykk Dypere)
Før Swift og moderne programmeringsspråk, var dato-manipulasjon tungvint og feilutsatt. Forskjeller i tidssoner og kalendersystemer gjorde sammenligninger komplekse. Swift's `Date` type, sammen med `DateFormatter`, tar hånd om mange av disse problemene automatisk.

Alternativt kan `Calendar` klassen brukes for å sammenligne datoer med mer kontekst, som å justere for tidssoner eller finne ut om to datoer er på samme dag.

Når det gjelder implementering, representerer `Date` et bestemt punkt i tid, ikke en "dato" i tradisjonell forstand. Tidspunkter er normalt lagret som tiden siden et referanseøyeblikk (f.eks. Unix epoch). Så, når vi sammenligner to `Date` objekter, sammenligner vi egentlig to tidspunkt.

## See Also (Se Også)
- Apple's Swift Documentation on Dates: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- Date and Time Programming Guide for Cocoa: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
- DateFormatter Class Reference: [https://developer.apple.com/documentation/foundation/dateformatter](https://developer.apple.com/documentation/foundation/dateformatter)
