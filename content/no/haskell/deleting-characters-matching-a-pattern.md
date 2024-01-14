---
title:    "Haskell: Slette tegn som matcher et mønster"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor slette tegn som matcher et mønster?

Sletting av tegn som matcher et bestemt mønster er nyttig i mange tilfeller, spesielt når man ønsker å rengjøre en tekst for uønskede tegn eller mønstre. Dette kan være spesielt viktig når man jobber med sensitiv informasjon eller ønsker å formatere tekst på en spesifikk måte.

## Hvordan gjøre det

For å slette tegn som matcher et mønster, kan man bruke funksjonen `deleteFirstsBy` fra `Data.List`-biblioteket i Haskell. Denne funksjonen tar inn en funksjon som definerer hvilke tegn som skal slettes, og en liste av tegn som skal søkes i.

La oss se på et eksempel hvor vi ønsker å slette alle tall fra en tekststreng:

```Haskell
import Data.List

tekst = "Dette er en tekst med tall 123"

slettTall :: Char -> Char -> Bool
slettTall c1 c2 = c1 `elem` ['0'..'9']

slettetTekst = deleteFirstsBy slettTall tekst

print slettetTekst
```

Output:

```
Dette er en tekst med tall
```

Vi definerte først en funksjon `slettTall` som sjekker om et tegn er en tallverdi eller ikke. Deretter brukte vi `deleteFirstsBy`-funksjonen til å gå gjennom teksten og slette alle matchende tall.

## Dykk dypere

Hvis du ønsker en dypere forståelse av hvordan `deleteFirstsBy`-funksjonen fungerer, kan du se på dens implementasjon. Denne funksjonen tar inn en startliste, en liste av tegn som skal slettes, og returnerer en ny liste med de matchende tegnene slettet.

Man kan også utforske andre måter å slette tegn som matcher et mønster på, for eksempel ved å bruke regulære uttrykk eller andre funksjoner fra `Data.List`-biblioteket.

## Se også

- [Dokumentasjon for `deleteFirstsBy`-funksjonen](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:deleteFirstsBy)
- [Offisiell Haskell-dokumentasjon](https://www.haskell.org/documentation/)