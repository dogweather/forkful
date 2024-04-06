---
date: 2024-01-20 17:38:33.253146-07:00
description: "Hvordan: Konvertering til sm\xE5 bokstaver har v\xE6rt en standard tekstbehandlingsoperasjon\
  \ lenge siden f\xF8r dator\xE6raen, brukt i leksikografi og indeksering. I\u2026"
lastmod: '2024-04-05T21:53:41.800477-06:00'
model: gpt-4-1106-preview
summary: "Konvertering til sm\xE5 bokstaver har v\xE6rt en standard tekstbehandlingsoperasjon\
  \ lenge siden f\xF8r dator\xE6raen, brukt i leksikografi og indeksering."
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

## Hvordan:
```Haskell
import Data.Char (toLower)

-- Eksempel funksjon for å konvertere en tekststreng til små bokstaver
toLowercase :: String -> String
toLowercase = map toLower

-- Prøv det ut!
main :: IO ()
main = putStrLn (toLowercase "Hei, Verden!")

-- Forventet output:
-- "hei, verden!"
```

## Dypdykk
Konvertering til små bokstaver har vært en standard tekstbehandlingsoperasjon lenge siden før datoræraen, brukt i leksikografi og indeksering. I Haskell, som i andre programmeringsspråk, gjøres konvertering lett ved hjelp av standardbiblioteket. Alternativene inkluderer bruk av regex for å finne og erstatte store bokstaver manuelt, men `Data.Char` (spesielt `toLower`) er veien å gå for renhet og enkelhet. Når det gjelder implementering, bruker `toLower` Unicode data for å finne de riktige småbokstavsvarene for en gitt bokstav.

## Se Også
- Haskell `Data.Char` dokumentasjon: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- En mer omfattende forståelse av tekstbehandling i Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
- Unicode standard for tekstbehandling: https://unicode.org/stds/unicode.html
