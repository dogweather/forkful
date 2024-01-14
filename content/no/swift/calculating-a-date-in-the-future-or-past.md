---
title:                "Swift: Beregning av en dato i fremtiden eller fortiden"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man utvikler en applikasjon, kan det være nyttig å kunne beregne og vise fremtidige eller tidligere datoer. Dette kan være nyttig for å lage en dynamisk kalenderfunksjon, eller for å vise brukerne når et spesifikt arrangement vil finne sted. Uansett årsak, vil lære å beregne datoer i Swift kunne hjelpe deg med å lage mer funksjonelle og tilpasningsdyktige applikasjoner.

## Hvordan

For å beregne en dato i fremtiden, kan vi bruke `Calendar` og `DateComponents` klassene i Swift. Først må vi velge datoen vi ønsker å starte fra. Dette kan gjøres ved å bruke `Date()` for å få dagens dato, eller ved å lage en `Date` objekt basert på en spesifikk dato. La oss si at vi vil beregne datoen 10 dager frem i tid:

```Swift
let dato = Date() // dagens dato
```

Deretter må vi opprette en instans av `Calendar` og `DateComponents` som vi kan bruke til å legge til 10 dager til datoen vår:

```Swift
let kalender = Calendar.current
var datoKomponenter = DateComponents()
datoKomponenter.day = 10 // legger til 10 dager
```

Til slutt må vi bruke `date(byAdding:to:)` metoden på `Calendar` for å få den nye, beregnede datoen:

```Swift
let beregnetDato = kalender.date(byAdding: datoKomponenter, to: dato)
```

For å beregne en dato i fortiden, følger vi samme prosess, men bruker et negativt tall for å trekke fra dager, måneder eller år fra den startdatoen vi har valgt.

## Deep Dive

Det er viktig å være klar over at datoen som returneres fra `date(byAdding:to:)` metoden kan avvike fra den nøyaktige dato og tiden du har spesifisert. Dette skyldes ulike faktorer som tidssoner og sommertid. Ved å bruke `date(byAdding:to:)` får vi en tilnærmet dato som tar hensyn til disse faktorene, noe som kan være nyttig for å unngå feil i kalkulasjonene våre.

## Se også

- [Beregn datoer i Swift](https://www.hackingwithswift.com/example-code/system/how-to-add-days-to-a-date-using-calendar)
- [Bruke DateComponents i Swift](https://developer.apple.com/documentation/foundation/datecomponents)
- [Apple Dokumentasjon om Calendar](https://developer.apple.com/documentation/foundation/urlsession/2097913-date)