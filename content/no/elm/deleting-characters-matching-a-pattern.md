---
title:                "Slette tegn som matcher et mønster"
html_title:           "Elm: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster er en nødvendig funksjon i mange programmeringsspråk, inkludert Elm. Dette kan hjelpe deg med å rydde opp i tekststrenger og fjerne uønskede tegn eller ord.

## Slik gjør du det

For å slette tegn som matcher et mønster i Elm, kan du bruke den innebygde funksjonen "String.filter". Denne funksjonen tar inn et mønster og en tekststreng, og returnerer en ny tekststreng hvor alle tegn som matcher mønsteret er slettet. Her er et enkelt eksempel på hvordan du kan bruke denne funksjonen:

```elm
-- Sletting av alle tall i en tekststreng
String.filter (\char -> not (Char.isDigit char)) "abc123def"
-- Output: "abcdef"
```

Du kan også bruke regex (regular expressions) for å identifisere og slette spesifikke mønstre i tekststrengen. Elm har en innebygd regex-modul som gjør dette enkelt. Her er et eksempel på hvordan du kan bruke regex for å slette alle punktum i en tekststreng:

```elm
import Regex exposing (..)

-- Sletting av alle punktum i en tekststreng
Regex.replace (Regex.regex "\\.") (\_ -> "") "abc.def.ghi"
-- Output: "abcdefghi"
```

## Dypdykk

Når du bruker "String.filter" til å slette tegn som matcher et mønster, må du være klar over at funksjonen også fjerner mellomrom og linjeskift. Dette kan være uønsket i visse situasjoner, for eksempel hvis du ønsker å fjerne enkelte ord eller uttrykk fra en tekststreng. I slike tilfeller kan det være bedre å bruke regex, som gir deg mer fleksibilitet. Du kan også kombinere regex-funksjoner som "replace" og "replacePattern" for å få mer avanserte slettemuligheter.

## Se også

- [Elm dokumentasjon for String.filter](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- [Elm dokumentasjon for Regex](https://package.elm-lang.org/packages/elm/regex/latest/)
- [En guide til regex i Elm](https://wilsonzhang2004.github.io/elm/2019/03/05/learn-regex-with-elm.html)