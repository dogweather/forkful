---
title:                "Swift: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i programmering må vi sammenligne forskjellige datoer. Dette kan være for å sjekke om en hendelse har skjedd før en annen, eller for å lage en sorteringsfunksjon basert på datoer. Uansett årsaken, er det viktig å kunne sammenligne to datoer nøyaktig og effektivt.

## Slik gjør du det

Å sammenligne to datoer i Swift er enkelt, takket være innebygde metoder og operatører. Her er et eksempel på hvordan du kan sammenligne to datoer og få en boolsk verdi tilbake som sier om den ene datoen er før eller etter den andre:

```Swift
let dato1 = Date() //dette vil lage en variabel som inneholder dagens dato og tid
let dato2 = Date(timeIntervalSinceNow: 86400) //dette vil lage en variabel som inneholder dato og tid 24 timer fra nå

if dato1 < dato2 {
    print("Dato 1 er før dato 2") //dette vil bli skrevet ut siden dagens dato er før dato og tid 24 timer fra nå
} else {
    print("Dato 2 er før dato 1")
}
```

I dette eksempelet bruker vi operatøren `<` for å sammenligne to datoer. Vi kan også bruke andre operatører som `>` (større enn), `==` (lik) og `!=` (ikke lik) for å utføre sammenligninger.

## Dypdykk

For å kunne sammenligne to datoer nøyaktig, er det viktig å forstå hvordan datoer og tid blir lagret og representert i Swift. Standard måten å lagre datoer og tid på er ved hjelp av `struct`en `Date`. Dette er en klasse som inneholder både dato og tid, og kan konverteres til og fra andre formater som `String` og `Int`. Det er også viktig å være klar over at dato og tid kan være påvirket av tidszoner, så det kan være lurt å bruke moderne løsninger som `DateFormatter` for å sikre nøyaktige sammenligninger uavhengig av tidszone.

## Se også

- [Offisiell Swift dokumentasjon for Date og DateComponents](https://developer.apple.com/documentation/foundation/date)
- [En bloggpost om å sammenligne datoer i Swift](https://medium.com/swift-programming/swift-4-1-comparing-dates-6139510357ee)
- [Tutorial om hvordan å bruke DateFormatter i Swift](https://www.ralfebert.de/ios/tutorials/date-time/)