---
title:                "Sammenligning av to datoer"
html_title:           "Swift: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Sammenligning av to datoer er en vanlig oppgave for programmerere. Det innebærer å sjekke om to datoer er lik, eller om den ene er større enn den andre.

Programmere gjør dette for å sammenligne hendelser eller for å sortere en liste av datoer etter dato.

# Slik gjør du det:

```Swift
let date1 = Date() // Oppretter en Date-instans som representerer nåværende tidspunkt
let date2 = Date(timeIntervalSinceReferenceDate: 86400) // Oppretter en Date-instans som representerer ett døgn siden

// Sammenligner datoer ved hjelp av ==, > og < operatørene
if date1 == date2 {
    print("Dato 1 og Dato 2 er like")
} else if date1 > date2 {
    print("Dato 1 er større enn Dato 2")
} else {
    print("Dato 1 er mindre enn Dato 2")
}
```

For å sammenligne datoer, kan du bruke == operatøren for å sjekke om to datoer er like. Du kan også bruke > eller < operatørene for å sjekke om en dato er større eller mindre enn en annen. Dette blir best illustrert med eksempelet over, hvor vi sammenligner nåværende dato med en dato som representerer ett døgn siden.

Output:

```
Dato 1 er større enn Dato 2
```

# Dykk dypere

Sammenligning av datoer har vært en viktig del av programmering i lang tid, spesielt når det gjelder å håndtere kalendere og tidsforskjeller. I tidligere språk som Java og C, var det vanlig å bruke spesifikke metoder for å sammenligne datoer. Men med Swift, kan du bruke de vanlige sammenligningsoperatørene som du allerede er kjent med.

Alternativt kan du bruke en DateComponents-instans for å få mer granulær kontroll over datoer og tider. Dette er spesielt nyttig når du må sammenligne datoer basert på ulike enheter, som måneder eller år.

Implementasjonen av sammenligning av datoer i Swift er basert på en grunnleggende tidsmåleenhet kalt “sekunder siden referansetidspunktet”. Dette referansetidspunktet er 1. januar 2001 kl. 00.00.00 GMT.

# Se også

For mer informasjon om datofunksjonalitet i Swift, kan du sjekke ut [Apple's offisielle dokumentasjon](https://developer.apple.com/documentation/foundation/date). Du kan også ta en titt på [NSDate vs NSDateComponents debate](https://stackoverflow.com/questions/25677448/nsdate-vs-nsdatecomponents) på Stack Overflow for å få en dypere forståelse av forskjellene mellom de to tilnærmingene for å sammenligne datoer.