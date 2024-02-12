---
title:                "Konvertere en streng til små bokstaver"
aliases:
- /no/haskell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:33.253146-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en tekststreng til små bokstaver betyr å gjøre alle bokstavene i strengen til miniatyrversjoner (små bokstaver). Programmerere gjør dette for å standardisere tekstdata, for eksempel når de sammenligner brukerinput eller sorterer data.

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
