---
title:    "Elm: Utvinning av substrenger"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor ekstrahere substrings i Elm

Det kan være en utrolig praktisk funksjon å være i stand til å ekstrahere substrings fra en streng i Elm. Dette kan være nyttig for å manipulere og bearbeide data, eller for å vise deler av en tekst på en mer leselig måte. Uansett hva årsaken er, kan substrings være en uvurderlig funksjon å ha i verktøykassen din som en Elm programmerer.

## Slik gjør du det i praksis

For å ekstrahere substrings i Elm, må du bruke funksjonen `String.slice` og angi start- og sluttposisjoner.

```Elm
String.slice start stop streng
```

La oss se på et eksempel der vi ønsker å ekstrahere de tre første bokstavene i strengen "katten min":

```Elm
String.slice 0 3 "katten min"
```

Dette vil gi oss resultatet "kat" som output. Merk at startposisjonen er inkludert, mens sluttposisjonen ikke er det.

Du kan også bruke en negativ verdi for å angi sluttposisjonen, slik at substrings blir ekstrahert fra slutten av strengen. Her er et eksempel på å ekstrahere de tre siste bokstavene i samme streng:

```Elm
String.slice 0 -3 "katten min"
```

Output vil da være "min" i dette tilfellet.

## Dypdykk i substrings

Som nevnt tidligere, kan substrings være nyttige for å manipulere data eller gjøre tekst mer leselig. Men det er også viktig å være oppmerksom på at det også kan være en utfordring å håndtere endringer i strengen du ekstraherer fra. Hvis start- eller sluttposisjonen endres, kan det hende at du ikke får den ønskede substrings som output.

En annen ting å huske på er at substrings funksjonen ikke fungerer på Unicode-tegn. Dette betyr at hvis strengen din inneholder Unicode-tegn, kan du oppleve uventede resultater når du prøver å ekstrahere substrings.

## Se også

- Offisiell Elm dokumentasjon for `String.slice`: https://package.elm-lang.org/packages/elm/core/latest/String#slice
- Andre nyttige funksjoner for å håndtere strenger i Elm: https://package.elm-lang.org/packages/elm/core/latest/String