---
title:    "Gleam: Ikke noe."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kapitalisere en streng kan virke som en liten og ubetydelig kodeøvelse, men det kan faktisk være svært nyttig i visse programmeringssituasjoner. Enten det er for å opprettholde en ensartet stil i teksten, eller for å fremheve viktige ord i en setning, kan kapitalisering av strenger være en nødvendig funksjonalitet.

## Hvordan

For å kapitalisere en streng i Gleam, kan du bruke funksjonen `String.capitalize`. Denne funksjonen tar inn en streng og returnerer samme streng, men med første bokstav store bokstaver. Her er et eksempel på hvordan du kan bruke denne funksjonen i Gleam:

```Gleam
let navn = "jeg elsker gleam"
let kapitalisert_navn = String.capitalize(navn)

// Output: "Jeg elsker gleam"
```

Som du kan se, vil funksjonen gi oss en kapitalisert versjon av strengen "jeg elsker gleam". Det er viktig å huske at denne funksjonen kun vil kapitalisere første bokstav i en streng, så hvis du ønsker å kapitalisere alle ord, må du bruke en annen funksjon eller en løkke.

## Dypdykk

Det kan være lurt å ha litt kunnskap om hvordan strenger fungerer i Gleam før du begynner å kapitalisere dem. I Gleam, som de fleste andre programmeringsspråk, er en streng en sekvens av tegn. Dette betyr at hver bokstav, symbol og mellomrom i en streng har en tilhørende numerisk verdi som datamaskinen kan forstå.

Når vi bruker funksjonen `String.capitalize`, vil den bruke disse numeriske verdiene til å endre noen av tegnene til store bokstaver. Dette er en enkel, men viktig funksjonalitet som kan hjelpe med å gi en profesjonell og konsistent stil til tekst i et program.

## Se også

- [Offisiell Gleam dokumentasjon](https://gleam.run/documentation/)
- [Kapitalisering av strenger i andre programmeringsspråk](https://www.programiz.com/dsa/program-to-capitalize-first-letter-of-each-word-in-sentence)
- [Mer om strenger i Gleam](https://gleam.run/articles/working-with-strings-in-gleam/)