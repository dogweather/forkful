---
title:                "Refaktorering"
date:                  2024-01-26T01:18:27.715005-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Refaktorisering er prosessen med å bearbeide koden din for å gjøre den renere, mer vedlikeholdbar, uten å endre dens eksterne oppførsel. Programmerere refaktorerer for å forbedre lesbarheten, redusere kompleksiteten og gjøre kodebasen mer mottakelig for fremtidige oppdateringer eller tilleggsfunksjoner.

## Hvordan:
La oss si at du har en del kode der du gjør noen gjentatte beregninger eller strengmanipulasjoner på tvers av flere funksjoner. Det er et primærmål for refaktorisering. Her er en før-og-etter ved bruk av Gleam, som legger stor vekt på typesikkerhet og immutabilitet:

```gleam
// Før refaktorisering
pub fn beregn_areal(bredde: Int, høyde: Int) -> Int {
  bredde * høyde
}

pub fn skriv_ut_areal(bredde: Int, høyde: Int) {
  let areal = beregn_areal(bredde, høyde)
  io.println("Arealet er \(areal)")
}

// Etter refaktorisering
pub fn beregn_areal(bredde: Int, høyde: Int) -> Int {
  bredde * høyde
}

pub fn skriv_ut_areal(areal: Int) {
  io.println("Arealet er \(areal)")
}

// I en annen del av koden din, vil du kalle på skriv_ut_areal slik:
skriv_ut_areal(beregn_areal(10, 20))
```

Eksempel på utdata:
```
Arealet er 200
```

Ved å refaktorisere har vi gjort `skriv_ut_areal` mer fokusert på bare å skrive ut, mens beregningen håndteres andre steder, noe som gjør koden mer modulær og lettere å gjenbruke eller teste.

## Dypdykk
Refaktorisering, som et konsept, har vært rundt så lenge som programmering selv — å gjenbesøke og rydde opp i kode er en del av god husstell. Den moderne formaliseringen av refaktorisering, sammen med mange av teknikkene og mønstrene som brukes i dag, kan spores tilbake til Martin Fowlers banebrytende bok "Refaktorisering: Forbedring av designet til eksisterende kode" utgitt i 1999.

I Gleam-økosystemet har refaktorisering spesifikke betraktninger. En av de mest betydningsfulle er den sterke typesjekkingen ved kompileringstid, som kan bidra til å fange opp feil tidlig når du flytter på ting. Gleams mønstergjenkjenning og immutabilitetsfunksjoner kan også lede deg til å skrive klarere, mer koncis kode — et av de primære målene med refaktorisering.

Alternativer til refaktorisering kan inkludere å skrive kode på nytt fra bunnen av eller lappe kode med raske fikser. Refaktorisering er imidlertid vanligvis den sikreste og mest effektive tilnærmingen til å forbedre eksisterende kode uten å introdusere nye feil, da det innebærer trinnvise, godt underbygde, oppførselsbevarende transformasjoner.

## Se også
- Martin Fowlers bok "Refaktorisering": https://martinfowler.com/books/refactoring.html
- Gleam-språkets nettsted, med ytterligere dokumentasjon og eksempler: https://gleam.run/
- "Refaktorisering: Forbedring av designet til eksisterende kode" av Martin Fowler (for underliggende prinsipper som gjelder på tvers av språk): https://martinfowler.com/books/refactoring.html