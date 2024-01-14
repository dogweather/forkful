---
title:    "Haskell: Stor bokstaving av en streng"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med tekstbehandling i Haskell, kan du støte på en oppgave der du trenger å konvertere en streng til store bokstaver. Dette kan være nyttig for å få et mer konsistent og leselig utseende på teksten.

## Hvordan

For å konvertere en streng til store bokstaver i Haskell, kan du bruke funksjonen `toUpper` fra modulet `Data.Char`. Dette er en standardfunksjon som allerede er inkludert i Haskell-biblioteket.

```Haskell
import Data.Char (toUpper)

toUpper "heisann" -- Output: "HEISANN"
```

Det er også mulig å bruke funksjonen `map` sammen med `toUpper` for å konvertere en liste av tegn til store bokstaver.

```Haskell
map toUpper "hei der" -- Output: "HEI DER"
```

En annen måte å konvertere en streng til store bokstaver på er å bruke funksjonen `toUpper` fra modulet `Data.Text`. Dette modulet er spesialisert for håndtering av tekst og kan gi bedre ytelse når det gjelder store tekstmengder.

```Haskell
import Data.Text (toUpper)

toUpper "dette er en tekst" -- Output: "DETTE ER EN TEKST"
```

## Dyp Dykk

Når vi ser nærmere på funksjonen `toUpper`, kan vi se at den tar inn en enkelt `Char` og returnerer en annen `Char`. Dette gjør at funksjonen bare kan brukes på enkelttegn, og ikke en hel streng.

Vi kan imidlertid bruke funksjonen `map` til å bruke `toUpper` på hvert tegn i en streng, slik vi gjorde i eksemplet ovenfor. Dette gjør at vi kan konvertere hele strengen til store bokstaver på en enkel måte.

Det er også verdt å nevne at funksjonene `toUpper` og `toLower` i Haskell bruker Unicode-standarden for å konvertere bokstaver. Dette betyr at de også vil fungere for bokstaver fra andre språk enn engelsk.

## Se også

- [Data.Char dokumentasjon](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Data.Text dokumentasjon](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Unicode dokumentasjon](https://www.unicode.org/)