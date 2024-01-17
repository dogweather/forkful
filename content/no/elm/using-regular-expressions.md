---
title:                "Å bruke regulære uttrykk"
html_title:           "Elm: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Bruk av regulære uttrykk, også kjent som regex, er et viktig verktøy for programmerere. Regex er en måte å søke og manipulere tekst på, og det er spesielt nyttig for å finne og erstatte mønstre i store tekstfiler. Så hvorfor bruker programmerere dette? Vel, regex kan hjelpe oss med å automatisere databehandling og forenkle søkeprosesser ved å bruke en enkel syntaks. Dette sparer oss for mye tid og manuelt arbeid.

## Hvordan:

La oss se på et eksempel på hvordan du bruker regex i Elm. Anta at du har en liste med e-postadresser og vil filtrere ut alle adresser som ikke tilhører Gmail-domenet. Her er koden du kan bruke:

```Elm
import Regex exposing (regex, find)
import String exposing (split)

emails = ["example1@ gmail.com", "example2@gmail.com", "example3@yahoo.com"]

filtered = List.filter (\email -> 
    case find (regex "gmail.com") email of
        Just _ -> True
        Nothing -> False
    ) emails

-- Output: ["example2@gmail.com"]
```

## Dykk dypere:

Regex har eksistert siden det ble introdusert i Perl-programmeringsspråket på 1980-tallet. Siden da har det blitt en standardfunksjon i mange programmeringsspråk, inkludert Elm. Alternativene til regex inkluderer manuelt søk eller bruk av innebygde funksjoner for tekstbehandling, men disse metodene kan være ineffektive og tidkrevende. I Elm kan du bruke methods fra biblioteket Regex for å utføre regex-operasjoner.

## Se også:

- Elm Regex-dokumentasjon: https://package.elm-lang.org/packages/elm/regex/latest/
- Regex tutorials: https://regexone.com/
- Sammenligning av regex i forskjellige programmeringsspråk: https://www.regular-expressions.info/comparison.html