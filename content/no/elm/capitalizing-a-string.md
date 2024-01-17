---
title:                "Konvertere en streng til stor bokstav"
html_title:           "Elm: Konvertere en streng til stor bokstav"
simple_title:         "Konvertere en streng til stor bokstav"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hva og Hvorfor?
Når vi snakker om å "kapitalisere" en streng i programmering, betyr det rett og slett å gjøre alle bokstavene i strengen store bokstaver (store bokstaver). Så hvis vi for eksempel har strengen "heIiVerDen", vil en kapitalisert versjon av denne strengen være "HEIIVERDEN". Programmerere gjør dette fordi det kan være nyttig å ha en standardisert versjon av en streng, spesielt når man jobber med input fra brukere.

# Slik gjør du det:
En kapitalisert streng kan enkelt opprettes ved å bruke funksjonen "String.toUpper" i Elm. La oss se på et eksempel:
```Elm
kapitalisertStreng = String.toUpper "heIiVerDen"
```
Dette vil gi oss outputen "HEIIVERDEN". Det er også mulig å kapitalisere individuelle ord eller setninger ved å bruke funksjonen "String.words" og deretter "List.map" for å anvende "String.toUpper" på hvert element i listen. For eksempel:
```Elm
kapitalisertOrd = String.words "heIiVerDen" |> List.map String.toUpper
```
Dette vil gi oss outputen ["HEIIVERDEN"]. 

# Dypdykk:
Kapitalisering av strenger har vært en vanlig praksis i programmering i lang tid, og brukes fortsatt mye i dag. Det finnes imidlertid noen alternativer som kan brukes, for eksempel å konvertere strenger til små bokstaver i stedet for store bokstaver. Dette kan være nyttig i visse tilfeller, for eksempel når man jobber med data som skal sammenlignes eller sorteres alfabetisk. Implementeringen av kapitalisering kan også variere avhengig av programmeringsspråk, men prinsippet er det samme.

# Se også:
- [Elm dokumentasjon om strenger](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Fordeler og ulemper med å kapitalisere strenger i programmering](https://www.geeksforgeeks.org/capitalize-first-letter-of-each-word-in-string/)
- [Andre nyttige strengmanipuleringsfunksjoner i Elm](https://package.elm-lang.org/packages/elm/core/latest/String)