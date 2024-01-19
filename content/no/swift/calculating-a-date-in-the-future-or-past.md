---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Swift: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Bruke Swift til å beregne fremtidige og tidligere datoer

## Hva og hvorfor?

Beregningsdatoer i fremtiden eller fortiden er operasjoner for å finne spesifikke datoer før eller etter en gitt dato. Programvareutviklere gjør det ofte for å håndtere reservasjoner, påminnelser, planlegging og tidshåndtering i programmene sine.

## Hvordan:

Vi vil bruke Swifts innebygde `Calendar` og `DateComponents` for å beregne nye datoer. Se på eksemplene nedenfor:

```swift
let nåværendeDato = Date()
var datoKomponenter = DateComponents()
datoKomponenter.day = 7
let kalender = Calendar.current
if let fremtidigDato = kalender.date(byAdding: datoKomponenter, to: nåværendeDato) {
    print("Fremtidig dato: \(fremtidigDato)")
}
```

I dette eksemplet vil output være en dato nøyaktig en uke fra nåværende dato.

## Dybdeplunge:

Det er flere måter å gjøre beregninger med datoer på i Swift, men bruk av `Calendar` og `DateComponents` er det mest brukervennlige og lesbare.

Historisk sett har programvareutviklere måttet håndtere komplekse tidssone-, kalender- og lokaliseringsspørsmål ved beregning av datoer. Swift prøver å forenkle dette gjennom bruken av høy-nivå API'er som kalender og dato-komponenter.

Et alternativ til denne tilnærmingen ville være å bruke `TimeInterval`, men dette krever gjerne mer arbeid for å håndtere ting som skuddår, tidszoner og månedslengder.

De viktigste detaljene i implementeringen er at `Calendar` og `DateComponents` automatisk betrakter tidssoner, lokale innstillinger og skuddår, noe som gjør håndtering av datoer mye mindre feilutsatt og tidsbesparende.

## Se også:

- [Apple Dato og Tid programmering guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
- [NSHipster artikkel om Swift datoer](https://nshipster.com/datecomponents/)

Hvis du ønsker å forstå mer om hvordan Swift håndterer datoer og tid, så anbefales det å utforske disse lenkene. Happy coding!