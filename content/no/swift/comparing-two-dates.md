---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer refererer til prosessen der programmereren bestemmer hvilken av to datoer som kommer først, eller om de er de samme. Dette trengs ofte for å sortere hendelser, håndtere tidsstemplede data eller regulere sekvenser og timing i apper.

## Hvordan:

Her er et grunnleggende eksempel på hvordan sammenligne to datoer i Swift:

```Swift
import Foundation

let dato1 = Date()
let dato2 = Date().addingTimeInterval(3600)

if dato1 < dato2 {
    print("Dato1 er tidligere enn Dato2")
} else if dato1 > dato2 {
    print("Dato1 er senere enn Dato2")
} else {
    print("Datoene er like")
}
```

Produktet av dette scriptet vil være: `"Dato1 er tidligere enn Dato2"` siden dato1 er satt til nåværende tidspunkt, mens dato2 er en time senere.

## Dyp Dykk:

Sammenligning av datoer har vært en del av programmering siden de tidligste dagene av beregning. Formatering og håndtering av dato- og klokkeslett er et kjent problemområde i mange programmeringsspråk, og Swift er intet unntak. Dato- og tidsforskjeller kan være triksy på grunn av tidssoner, skuddår og dato-overganger.

Alternativene til `<` og `>` for sammenligning av datoer, inkluderer `compare(_:)` metoden og `timeIntervalSince(_:)`. Disse gir mer fleksibilitet, men det er mer komplisert å bruke.

Implementeringsdetaljer: Swift `Date`-objekter er lagret internt som antall sekunder som har gått siden en fast dato og tid, kjent som "epoken". Standard epoken er 1. januar 1970 kl 00:00 UTC.

## Se også:

1. Swift Date Klasseforskrift: [Link](https://developer.apple.com/documentation/foundation/date)
2. Håndtering av Dato og Tid i Swift: [Link](https://www.swiftbysundell.com/articles/handling-date-and-time-in-swift/)
3. Sammenlign Datoer med Swift: [Link](https://useyourloaf.com/blog/swift-date-comparison/)