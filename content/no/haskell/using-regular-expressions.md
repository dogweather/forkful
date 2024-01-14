---
title:    "Haskell: Å bruke regulære uttrykk"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bruke regulære uttrykk i Haskell? Regulære uttrykk er et kraftig verktøy for å behandle og manipulere tekst. Det gjør det enklere å finne og endre mønstre i en tekststreng, noe som kan være nyttig i mange forskjellige programmeringsscenarier.

## Hvordan

La oss se på noen eksempler på hvordan du kan bruke regulære uttrykk i Haskell. Vi starter med å importere "Text.Regex" biblioteket.

```Haskell
import Text.Regex
```

### Finn et enkelt mønster

La oss si at vi har en tekststreng og vil finne ut om den inneholder ordet "hei". Vi kan bruke funksjonen `match` fra "Text.Regex" biblioteket.

```Haskell
match (mkRegex "hei") "Hei, hvordan har du det?" -- output: Just ([("hei",0,3)],"",", hvordan har du det?")
```

Funksjonen `match` tar to parametere: et regulært uttrykk og en tekststreng. Den returnerer en verdi av typen `Maybe (MatchText String)`, som enten kan være `Just` en tupel med informasjon om samsvarende deler av tekststrengen, eller `Nothing` hvis det ikke finnes noe samsvar.

### Erstatt et mønster

Vi kan også bruke regulære uttrykk til å erstatte en del av en tekststreng med en annen del. La oss si at vi har en tekststreng som inneholder en feilstavelse, og vi vil bytte ut feilstavelsen med det riktige ordet.

```Haskell
subRegex (mkRegex "beleiver") "Du er en beleiver" "believer" -- output: "Du er en believer"
```

Her bruker vi funksjonen `subRegex` til å bytte ut "beleiver" med "believer" i tekststrengen.

## Dykk dypere

Dette er bare en grunnleggende introduksjon til hvordan du kan bruke regulære uttrykk i Haskell. Du kan lese mer om de ulike funksjonene og mulighetene i "Text.Regex" biblioteket i Haskell-dokumentasjonen.

## Se også

- [Haskell-dokumentasjonen for "Text.Regex"](https://hackage.haskell.org/package/regex-base)
- [En hurtigreferanse for regulære uttrykk i Haskell](https://gist.github.com/jonschoning/3031394)