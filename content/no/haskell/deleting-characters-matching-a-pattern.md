---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Haskell: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med tekstbehandling i Haskell, kan det være nødvendig å slette visse tegn som matcher et bestemt mønster. Dette kan være nyttig for å filtrere eller endre tekst på en enkel måte. 

## Hvordan

Hvis du for eksempel ønsker å slette alle tall fra en tekststreng, kan du bruke funksjonen `deleteBy` sammen med et tilpasset predikat. Predikatet vil avgjøre hvilke tegn som skal slettes basert på et bestemt mønster. For eksempel, hvis du vil slette alle tall, kan du bruke `isDigit` fra `Data.Char` biblioteket som predikatet.

```Haskell
-- Importer Data.Char for å få tilgang til isDigit funksjon
import Data.Char

-- Funksjon for å slette alle tall fra en tekststreng
deleteDigits :: String -> String
deleteDigits = deleteBy isDigit

-- Eksempel på bruk
deleteDigits "abc123def456ghi"   -- "abcdefghi"
```

## Dypdykk

Det finnes også andre måter å slette tegn som matcher et mønster på, som for eksempel å bruke funksjonen `filter` fra `Data.List` biblioteket. Denne funksjonen tar inn en liste og et predikat, og returnerer en liste med bare de elementene som matcher predikatet. Du kan også bruke regulære uttrykk med `Regex` biblioteket for mer avansert mønstermatching.

## Se Også

- [Haskell Dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell Wiki](https://wiki.haskell.org/)