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

## Hva & Hvorfor?

Beregning av en dato i fremtiden eller fortiden er en prosess der en programmerer bruker kodespråket Swift for å beregne en spesifikk dato basert på et gitt antall dager, uker, måneder eller år frem eller tilbake i tid. Dette er en viktig teknikk for å håndtere datoer og tidsberegningsrelaterte problemer i programmering.

## Slik gjør du det:

For å beregne en dato i fremtiden eller fortiden i Swift, bruker du metoden `addingTimeInterval()` på et `Date`-objekt. Denne metoden tar inn et flytende desimaltall som representerer antall sekunder å legge til eller trekke fra fra den nåværende datoen. Her er et eksempel på hvordan du kan beregne datoen tre måneder frem i tid:

```Swift
let currentDate = Date()
let threeMonths = TimeInterval(60*60*24*30*3) // antall sekunder i 3 måneder
let futureDate = currentDate.addingTimeInterval(threeMonths)

print(futureDate)

// Output:
// 2021-06-13 17:18:54 +0000
```

## Dypdykk:

Historisk sett har beregning av datoer vært en kompleks oppgave for programmerere, spesielt på grunn av ulike kalendersystemer og måter å representere datoer på. I dag er det flere programmeringsbiblioteker tilgjengelig som gjør denne prosessen enklere og mer nøyaktig. Noen alternative metoder for å beregne datoer inkluderer `date(byAdding:to:wrappingAround:)`-metoden i Foundation frameworket og `date(byAdding:withValue:to:wrappingComponents:)`-metoden i Calendar-classen. Implementeringsdetaljene for disse metodene kan variere, men konseptet og bruken av dem er ganske lik metoden vi brukte i eksempelet over.

## Se også:

For mer informasjon om hvordan du arbeider med datoer i Swift, kan du se Apples offisielle dokumentasjon og ulike online ressurser, som for eksempel Ray Wenderlichs tutorial om håndtering av datoer i Swift. Husk å alltid være nøye med data og datoer når du arbeider med programmering, da unøyaktigheter i dette området kan føre til alvorlige feil i koden din.