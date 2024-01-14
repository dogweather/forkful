---
title:    "Elm: Finne lengden av en streng"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Det å finne lengden til en streng kan være en nyttig oppgave i mange programmeringsscenarier. Enten du arbeider med tekstbehandling eller databehandling, vil du sannsynligvis på et tidspunkt trenge å få tilgang til lengden til en streng. I denne bloggposten vil vi se på hvordan du kan gjøre dette i Elm.

## Slik gjør du det

For å finne lengden til en streng i Elm, kan du bruke funksjonen `String.length`. Her er et eksempel på hvordan du kan bruke denne funksjonen i praksis:

```elm
string = "Dette er en streng"
stringLength = String.length string
```

I dette tilfellet vil variabelen `stringLength` inneholde verdien 18, som er lengden til strengen vi har definert.

## Dykk ned i detaljene

For å virkelig forstå hvordan `String.length` fungerer, er det nyttig å vite at i Elm er en streng et alias for en liste av tegn (characters). For å finne lengden av en liste, kan vi bruke funksjonen `List.length`. Når vi kaller funksjonen `String.length` på en streng, konverteres den automatisk til en liste, og deretter returneres lengden basert på hvor mange tegn den inneholder.

## Se også

Her er noen nyttige ressurser for å lære mer om å finne lengden til en streng i Elm:

- Om `String.length` funksjonen: [Elm dokumentasjon](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- Om konvertering av strenger til lister: [Kodingseventyr blogg](https://kodingseventyr.no/elm-convert-string-to-list/)
- Om elm-strengbiblioteker: [Elm Package Index](https://package.elm-lang.org/packages/elm-community/elm-string-extra/latest/String-Extra)