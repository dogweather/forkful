---
date: 2024-01-26 04:45:56.437962-07:00
description: "Komplekse tall har en reel del og en imagin\xE6r del (som 3 + 4i). Programmerere\
  \ bruker dem i Swift for oppgaver som signalbehandling, l\xF8sning av visse\u2026"
lastmod: '2024-03-13T22:44:41.135954-06:00'
model: gpt-4-0125-preview
summary: "Komplekse tall har en reel del og en imagin\xE6r del (som 3 + 4i)."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

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
