---
title:                "Bruke regulære uttrykk"
html_title:           "Elm: Bruke regulære uttrykk"
simple_title:         "Bruke regulære uttrykk"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?

Regulære uttrykk er et kraftig verktøy for å matche og manipulere tekst. Programmerere bruker dem for å gjenkjenne, dele, erstatte og analysere data effektivt.

## Hvordan skal du:

```Elm
import Regex exposing (contains, regex)

-- Oppretter et regulært uttrykk
emailRegEx = regex "[a-zA-Z1-9._%+-]+@[a-zA-Z1-9.-]+\\.[A-Za-z]{2,}"

-- Test om en streng inneholder dette mønsteret
contains emailRegEx "min.email@eksempel.com" --> True
contains emailRegEx "ikke en email" --> False
```
## Dypere dykk

Regulære uttrykk ble først introdusert på 1950-tallet og har siden da blitt en fast komponent i de fleste programmeringsspråkene. I Elm, er `Regex` modulen den primære kilden for regex funksjonalitet. Alternativer til bruk av regulære uttrykk inkluderer string manipuleringsmetoder og eksterne biblioteker. Skjønt, valget mellom disse metodene avhenger hovedsakelig av spesifikasjoner av problemet du prøver å løse.

Enkelte viktige detaljer vedrørende utnyttelse av regulære uttrykk i Elm inkluderer deres flertydighet med hensyn til escaping og matchegenskaper, samt byrden av ytelsen når de brukes inept.

## Se også:

For en detaljert opplæring på bruk av regulære uttrykk i Elm, sjekk følgende lenker:

- Elm's Regex Modul Dokumentasjon [her](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- For en mer omfattende forklaring på regulære uttrykk og hvordan man bruker dem i programmering, se [Regulære Uttrykk for Nybegynnere](https://www.regular-expressions.info/tutorial.html)