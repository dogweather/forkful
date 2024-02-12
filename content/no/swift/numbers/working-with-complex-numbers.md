---
title:                "Å jobbe med komplekse tall"
aliases:
- /no/swift/working-with-complex-numbers.md
date:                  2024-01-26T04:45:56.437962-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med komplekse tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Komplekse tall har en reel del og en imaginær del (som 3 + 4i). Programmerere bruker dem i Swift for oppgaver som signalbehandling, løsning av visse matematiske problemer, og simulering av fysikk.

## Hvordan gjøre det:
Swift har ikke innebygd støtte for komplekse tall, men vi kan lage vårt eget:

```Swift
struct KomplekstTall {
    var reel: Double
    var imaginær: Double
    
    func leggTil(_ annet: KomplekstTall) -> KomplekstTall {
        return KomplekstTall(reel: reel + annet.reel, imaginær: imaginær + annet.imaginær)
    }
    
    // Tilleggs metoder som subtraksjon, multiplikasjon, osv.
}

let første = KomplekstTall(reel: 2, imaginær: 3)
let andre = KomplekstTall(reel: 1, imaginær: 4)
let resultat = første.leggTil(andre)
print("Resultat: \(resultat.reel) + \(resultat.imaginær)i")
// Eksempelutskrift: Resultat: 3.0 + 7.0i
```

## Dypdykk
Komplekse tall dukket opp i det 16. århundre i algebraiske ligninger. De er essensielle i kvantemekanikk, kontrollteori, og mange andre felt. Apples Swift har ikke et standard bibliotek for komplekse tall, i motsetning til språk som Python eller C++. Alternativer til å rulle ditt eget inkluderer bruk av Numerics-pakken som inkluderer støtte for komplekse tall eller innpakning av C++ kompleks bibliotek med Swifts interoperabilitet.

## Se også
- Swift Numerics: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
