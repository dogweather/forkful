---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Haskell: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en streng til små bokstaver kan være nyttig i mange tilfeller. Det kan hjelpe med å sammenligne strenger, filtrere data og generelt gjøre tekstbehandling enklere. I denne artikkelen skal vi se på hvordan man kan gjøre dette i Haskell.

## Hvordan gjøre det

For å konvertere en streng til små bokstaver i Haskell, kan vi bruke funksjonen `toLower` fra biblioteket `Data.Char`. Denne funksjonen tar inn en enkelt bokstav og returnerer bokstaven i små bokstaver. For å konvertere en hel streng, kan vi bruke funksjonen `map` som tar inn en funksjon og en liste, og returnerer en liste hvor funksjonen er brukt på hvert element i listen.

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString = map toLower
```

Vi definerer en ny funksjon `toLowerString` som tar inn en streng og bruker `map` for å bruke `toLower` på hver bokstav i strengen.

La oss prøve ut denne funksjonen med noen eksempler:

```Haskell
toLowerString "HELLO WORLD!" -- Output: "hello world!"
toLowerString "Haskell er gøy!" -- Output: "haskell er gøy!"
```

Som vi kan se, blir alle bokstavene i strengen konvertert til små bokstaver.

## Dypdykk

Det er viktig å merke seg at denne funksjonen bare vil fungere for ASCII-tegn, altså bokstaver i det latinske alfabetet. Dersom vi ønsker å konvertere strenger som inneholder andre språk eller spesialtegn, må vi bruke en annen funksjon som støtter dette. En slik funksjon er `map toLower . map toTitle` som følger:

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString = map toLower . map toTitle
```

Denne funksjonen vil først konvertere alle bokstavene til stor bokstav ved hjelp av `toTitle`, og deretter konvertere dem til små bokstaver med `toLower`. Dette vil gi korrekt konvertering for alle typer bokstaver.

## Se også

- "Introduction to Haskell" fra W3Schools: https://www.w3schools.com/haskell/
- "Data.Char" dokumentasjon fra Haskell.org: https://hackage.haskell.org/package/base/docs/Data-Char.html
- "Funksjoner i Haskell" fra Programiz: https://www.programiz.com/haskell-programming/functions