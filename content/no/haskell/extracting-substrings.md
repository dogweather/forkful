---
title:    "Haskell: Uttrekk av understrenger"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang funnet deg selv jobbe med en lang tekststreng og ønsket å bare få ut en del av den? Det er her substrings kommer til unnsetning! Ved å utvinne deler av en tekststreng, kan du få tilgang til spesifikke deler av teksten og utføre forskjellige operasjoner på dem.

## Hvordan

La oss si at du har en tekststreng som inneholder navnet ditt, og du vil få ut bare etternavnet. Det er her funksjonen `substring` i Haskell kommer til nytte. Ved å bruke denne funksjonen, kan du angi start- og sluttposisjonen til substringen du vil hente ut.

```Haskell
substring 7 12 "Ola Nordmann" -- output: "Nordmann"
```

Du kan også bruke en variabel for å styre start- og sluttposisjonen i stedet for å bruke tallverdier.

```Haskell
let start = 7
let end = 12
substring start end "Ola Nordmann" -- output: "Nordmann"
```

Det er også mulig å få ut alle tegn etter en bestemt posisjon ved å bare angi startposisjonen og la sluttposisjonen være `length`, som vil hente ut alt fra startposisjonen til slutten av teksten.

```Haskell
substring 5 length "Ola Nordmann" -- output: "Nordmann"
```

## Dykk ned

Det er viktig å merke seg at i Haskell, som i de fleste funksjonelle programmeringsspråk, er indekseringen basert på 0, så den første posisjonen i en tekststreng er faktisk 0, ikke 1. Dette kan føre til forvirring hvis du er vant til å jobbe med språk der indekseringen starter fra 1.

En annen ting å merke seg er at den siste posisjonen i substring må være mindre enn lengden på teksten. Ellers vil du få en feilmelding.

For å unngå feil, er det en god praksis å bruke funksjonen `length` for å få lengden på teksten og bruke det til å kontrollere start- og sluttposisjonen din før du henter ut en substring.

## Se også

[Offisiell Haskell-dokumentasjon for substring-funksjonen](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#v:substring)

[Haskell for Dummies: Substring Tutorial](https://www.dummies.com/programming/haskell/how-to-work-with-substrings-in-haskell/)