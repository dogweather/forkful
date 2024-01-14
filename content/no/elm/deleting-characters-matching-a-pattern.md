---
title:    "Elm: Sletting av tegn som stemmer med et mønster"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor
Lurer du på hvorfor noen ville engasjere seg i å slette tegn som matcher et mønster? Det kan være mange grunner til dette, som å rydde opp i data eller formatere tekst på en mer effektiv måte.

## Slik gjør du det
For å slette tegn som matcher et mønster i Elm, kan du bruke funksjonen `String.filter` sammen med en `isMatch` funksjon som tar inn et tegn og returnerer en boolsk verdi avhengig av om tegnet matcher mønsteret du ønsker å slette.

```Elm
import String exposing (..)

isMatch: Char -> Bool
isMatch c =
    -- Legg til mønsteret ditt her

filteredString: String -> String
filteredString str =
    filter isMatch str

main: Program
main =
    text (filteredString "Hei! Denne setningen inneholder noen tall: 123abc")  -- Returnerer "Hei! Denne setningen inneholder noen tall: abc"
```

## Dypdykk
Nå som du har fått et enkelt eksempel på hvordan du kan slette tegn som matcher et mønster, kan du også utforske andre måter å gjøre det på. Du kan for eksempel bruke `String.replace` for å erstatte tegnene med et annet tegn, eller jobbe med regex for mer avanserte mønstre.

## Se også
- [Elm dokumentasjon for String](https://package.elm-lang.org/packages/elm/string/latest/String)
- [Regex i Elm](https://package.elm-lang.org/packages/elm/regex/latest/)