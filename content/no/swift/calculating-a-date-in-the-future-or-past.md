---
title:    "Swift: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden er en grunnleggende del av programmering, uansett om du utvikler spill, mobile applikasjoner eller nettsider. Det kan være nyttig for å beregne forfallsdatoer, visning av tidssoner eller planlegging av hendelser. I denne blogginnlegget vil jeg dele hvordan du kan gjøre dette ved hjelp av Swift-programmeringsspråket.

## Hvordan

Først må vi opprette en NSDate-variabel. Dette gjøres ved å bruke konstruktøren "init(timeIntervalSinceReferenceDate:)". Det er viktig å huske at datoer beregnes i sekunder siden 1. januar 2001, så vi må konvertere dagen vi ønsker å beregne til sekunder ved å bruke NSDate()-funksjonen.

```Swift
let nå = NSDate()

// Dette er 1. januar 2030 i sekunder siden 1. januar 2001
let fremtidigDato = NSDate(timeIntervalSinceReferenceDate: 1893456000)
```

Vi kan nå bruke en rekke innebygde funksjoner i Swift for å beregne forskjellen mellom disse to datoene og vise resultatet i ønsket format. For eksempel kan vi bruke følgende kode for å vise antall dager mellom disse to datoene:

```Swift
let formatter = DateComponentsFormatter()
formatter.allowedUnits = .day
let resultat = formatter.string(from: nå, to: fremtidigDato) // Output: 3286 dager
```

Det finnes også mange andre formatteringsalternativer, slik som å vise tiden i timer, minutter eller til og med uker. Så det er viktig å lese dokumentasjonen for å finne ut hvilken som passer best for ditt behov.

## Dype dykk

Men hvorfor må vi konvertere datoen til sekunder? Dette skyldes at alle datofunksjonene i Swift brukes med utgangspunkt i et referansepunkt. I dette tilfellet er det 1. januar 2001, men referansepunktet kan variere fra programmeringsspråk til programmeringsspråk. Ved å konvertere datoer til sekunder, blir det enklere å beregne tidsforskjellen uavhengig av referansepunktet.

## Se også

- [Apple Documentation: Date Components Formatter](https://developer.apple.com/documentation/foundation/datecomponentsformatter)
- [Stack Overflow: Calculating Difference between two NSDates in years, months, days](https://stackoverflow.com/questions/13290660/calculating-difference-between-two-nsdates-in-years-months-days)
- [Raywenderlich.com: Calculating Dates in Swift](https://www.raywenderlich.com/83992/pro-tip-calculating-dates-swift)