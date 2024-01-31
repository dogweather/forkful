---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:38:56.519477-07:00
simple_title:         "Tolke en dato fra en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av dato fra en streng betyr å gjøre om teksten som representerer en dato til et dato-objekt. Programmører trenger dette for å enkelt manipulere datoer og sammenligne tidspunkter.

## Slik Gjør Du:
Swift håndterer datoer med `Date`, og for å parse en streng til en dato, bruker vi `DateFormatter`. Sett riktig datoformat og send strengen til formatteren.

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"
if let parsedDate = dateFormatter.date(from: "2021-05-20T15:30:00+0000") {
    print("Datoen er \(parsedDate)")
} else {
    print("Kunne ikke parse dato fra strengen")
}
```

Dette vil gi utskriften:

```
Datoen er 2021-05-20 15:30:00 +0000
```

Og husk, formateringen må matche strengen nøyaktig!

## Deep Dive
Tidligere i Swift-brukte folk ofte `NSDateFormatter`, som nå er `DateFormatter`. Det er viktig fordi string-representasjon av datoer endrer seg med kultur og språk. `DateFormatter` støtter lokalisering og kan konvertere til og fra tekst basert på brukerens lokalesettings.

Alternativer til `DateFormatter` inkluderer å bruke tredjepartsbiblioteker som `SwiftDate` eller parsing manuelt – men det siste anbefales ikke pga kompleksitet og feilrisiko.

Implementeringsdetaljer:
- Sett `locale` på `DateFormatter` til `Locale(identifier: "en_US_POSIX")` for å håndtere Edge-cases.
- Unngå parsing i GUI-thread fordi det kan være ressurskrevende.
- `ISO8601DateFormatter` er en spesiell formatter for ISO 8601 datoer.

## Se Også
- Swift-dokumentasjon om `DateFormatter`: [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- ISO 8601 Dato og tid på internett: [Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
- SwiftDate, et kraftig dato-tidsbibliotek: [SwiftDate GitHub](https://github.com/malcommac/SwiftDate)
