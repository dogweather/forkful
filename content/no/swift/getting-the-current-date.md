---
title:                "Hente nåværende dato"
html_title:           "Swift: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å få dagens dato i programmering refererer til å få informasjon om den nåværende datoen og tidspunktet. Dette er nyttig for å føre protokoll, planlegge hendelser og generelt for å holde orden på tidsstempler. Programmere trenger å få dagens dato for å sikre at systemene og applikasjonene deres fungerer riktig og nøyaktig.

## Slik

Her er et eksempel på hvordan du kan få dagens dato i Swift:

```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy HH:mm"
let currentDate = formatter.string(from: date)

print("Dagens dato er: \(currentDate)")
```

Dette vil få dagens dato og tidspunkt og formatere det som et string-objekt. Deretter skrives det ut i konsollen, og du vil få følgende utgang:

```Swift
Dagens dato er: 24.09.2021 14:30
```

Du kan også få dagens dato i en annen datoformat ved å endre `dateFormat` til en annen verdi. Det finnes mange ulike datoformateringsalternativer, slik at du kan tilpasse det til dine behov.

## Dypdykk

I programmering har det alltid vært viktig å jobbe med datoer og tider. Tidligere kunne det være en utfordring å få nøyaktig dato og tid på grunn av forskjellige tidsformater og systeminnstillinger. I dag er det mye enklere takket være et standardisert system for å håndtere datofunksjonalitet.

Som en alternativ måte å få dagens dato på, kan du også bruke `Foundation` biblioteket og bruke `Date()` og `DateFormatter()` metoder.

Implementeringen av å få dagens dato og tid er avhengig av operativsystemet og datofunksjonaliteten som er tilgjengelig. I Swift kan du også bruke tredjepartsbiblioteker for å få mer avanserte funksjoner for håndtering av dato og tid.

## Se også

For mer informasjon om hvordan du kan håndtere dato og tid i Swift, kan du se følgende ressurser:

- [Uoffisiell Swift Style Guide](https://swift.org/documentation/api-design-guidelines/#date-and-time)
- [Apple's Swift programmeringsguide](https://developer.apple.com/documentation/foundation/date)
- [Tredjepartsbiblioteker](https://cocoapods.org/search?q=date+and+time) for dato og tid håndtering i Swift.