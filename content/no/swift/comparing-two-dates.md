---
title:                "Sammenligner to datoer"
html_title:           "Swift: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Du lurer kanskje på hvorfor det er viktig å sammenligne to datoer? Vel, det er viktig når du jobber med datoer og tid i programvareutvikling. Å sammenligne datoer lar deg bla gjennom og manipulere data på en effektiv måte, og det er essensielt i mange applikasjoner.

## Hvordan gjøre det
Å sammenligne to datoer i Swift er enkelt og intuitivt. La oss ta en titt på noen kodeeksempler for å se hvordan det gjøres:

```Swift
// Oppretter to datoer
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: -3600) // En time siden

// Sjekker om date1 kommer før date2
if date1 < date2 {
    print("date1 kommer før date2")
} else {
    print("date2 kommer før date1")
}

// Sjekker om datoene er like
if date1 == date2 {
    print("datoene er like")
} else {
    print("datoene er ulike")
}

// Sjekker om date1 kommer etter date2
if date1 > date2 {
    print("date1 kommer etter date2")
} else {
    print("date2 kommer etter date1")
}
```

Dette vil gi følgende utskrift:

```
date1 kommer etter date2
datoene er ulike
date2 kommer før date1
```
Som du kan se, brukes operatørene `<`, `==` og `>` for å sammenligne datoer. Dette gjøres ved å sammenligne tidspunktet som datoene representerer.

## Dykk dypere
La oss gå litt dypere og se på hvordan datoer faktisk sammenlignes i Swift. Under kappen, bruker Swift POSIX-tidsstempel til å representere datoer og tidspunkt. Dette er antall sekunder siden 1. januar 1970, og det brukes overalt i Unix-systemer for å lagre datoer og tidspunkt.

Når du sammenligner to datoer, konverteres de til sine respektive tidsstempel og sammenlignes deretter. Dette gjøres ved hjelp av forskjellige metoder og egenskaper som er definert i Swifts `Date`-struktur. Disse inkluderer `timeIntervalSince1970` for å få tidsstempel og `compare()` for å sammenligne to datoer.

Det er også verdt å nevne at tidsstempel bare tar høyde for dato og tid, og ikke faktiske tidszoner eller sommertidjusteringer. Derfor er det viktig å være klar over disse faktorene når du arbeider med datoer i Swift.

## Se også
For mer informasjon om sammenligning av datoer i Swift, kan du se dokumentasjonen for `Date`-strukturen og dens metoder og egenskaper. Du kan også sjekke ut disse nyttige ressursene:

- [Offisiell Swift-dokumentasjon](https://developer.apple.com/documentation/swift/date)
- [Swift by Sundell: Datoer og tider](https://www.swiftbysundell.com/basics/dates-and-times/)

Takk for at du leste denne artikkelen, og vi håper den har hjulpet deg med å forstå hvordan å sammenligne datoer i Swift!