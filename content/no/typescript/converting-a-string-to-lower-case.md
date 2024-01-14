---
title:                "TypeScript: Konvertere en streng til små bokstaver"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens programmeringsverden, er det utrolig viktig å kunne jobbe med tekststrenger på en effektiv måte. Å konvertere en tekststreng til små bokstaver er en grunnleggende oppgave som kan forbedre koden din og gjøre den mer lesbar. Derfor er det viktig å forstå hvordan du utfører denne konverteringen i TypeScript.

## Hvordan gjøre det

Koden nedenfor viser deg hvordan du enkelt og effektivt kan konvertere en tekststreng til små bokstaver i TypeScript:

```TypeScript
let navn = "ANDERS";
console.log(navn.toLowerCase());
```

Output vil være:
`anders`

Som du kan se, bruker vi funksjonen `toLowerCase()` på tekststrengen `navn` for å konvertere alle bokstavene til små bokstaver. Dette resulterer i en mer lesbar og enhetlig kode.

## Dykk dypere

Men hva skjer egentlig bak kulissene når vi bruker `toLowerCase()` funksjonen? I TypeScript, og de fleste andre programmeringsspråk, er strenger uforanderlige, eller "immutable". Det betyr at når en streng er opprettet, kan du ikke gjøre endringer på den. Derfor må vi bruke funksjoner som `toLowerCase()` for å lage en ny streng med ønsket formatering. Denne funksjonen går gjennom hver bokstav i strengen og konverterer hvert tegn til tilsvarende små bokstav. Det er derfor viktig å huske at den opprinnelige tekststrengen forblir uforandret.

## Se også

Her er noen nyttige ressurser for å lære mer om å jobbe med tekststrenger i TypeScript:

- [Offisiell TypeScript dokumentasjon om strenge-manipulering](https://www.typescriptlang.org/docs/handbook/declaration-files/do's-and-don'ts.html#functions)
- [Stack Overflow - Konverter en tekststreng til små bokstaver i TypeScript](https://stackoverflow.com/questions/34482118/how-to-make-a-string-all-lowercase-in-typescript)

Takk for at du leste! Vi håper denne artikkelen har vært nyttig for deg i arbeidet med tekststrenger i TypeScript. Lykke til videre med kodingen! 😊