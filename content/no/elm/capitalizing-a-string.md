---
title:                "Elm: Å konvertere en streng til store bokstaver"
simple_title:         "Å konvertere en streng til store bokstaver"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

I dag skal vi snakke om en viktig funksjon i programmering - hvordan å kapitalisere en streng (string) i Elm. Dette er nyttig for å formatere tekster som overskrifter eller navn i applikasjoner. Men hvorfor skulle du trenge å kapitalisere en streng i første omgang?

Det kan være flere grunner til dette. Kanskje du ønsker å presentere data på en mer strukturert og estetisk måte. Eller kanskje du ønsker å gjøre teksten mer synlig for brukeren din. Uansett hva årsaken er, er evnen til å kapitalisere en streng viktig i mange situasjoner.

## Hvordan

For å kapitalisere en streng i Elm, kan du bruke funksjonen `String.toUpper`. La oss se på et eksempel på hvordan dette fungerer:

```Elm
import String

name = "nico"

capitalizedName = String.toUpper name

```
Output: "NICO"

Her har vi importert `String` modulen og brukt funksjonen `toUpper` på strengen "nico", som resulterer i at den blir kapitalisert til "NICO". Enkelt, ikke sant?

Du kan også bruke funksjonen `String.toLower` for å gjøre en streng til små bokstaver.

```Elm
import String

name = "NICOLE"

lowerName = String.toLower name

```
Output: "nicole"

Det er også verdt å nevne at disse funksjonene vil fungere på enhver streng, uavhengig av språk eller alfabet.

## Dypdykk

Dette var en enkel og rask måte å kapitalisere en streng i Elm, men det er også viktig å forstå hva som skjer bak kulissene. `String.toUpper` og `String.toLower` funksjonene bruker Unicode-tegnsettet for å konvertere bokstavene til store eller små. Dette betyr at de vil fungere for alle språk og spesialtegn.

Det er også verdt å nevne at `String.toUpper` og `String.toLower` ikke endrer originalstrengen, men returnerer en ny streng med den konverterte teksten.

## Se også

For mer informasjon om strenger og hvordan de fungerer i Elm, kan du sjekke ut følgende ressurser:

- [Elm offisiell dokumentasjon om strenger](https://elm-lang.org/docs/strings)
- [Elm tutorial på Strenger fra Ellie](https://ellie-app.com/new)
- [GitHub repository for String modulen i Elm](https://github.com/elm/core/tree/master/src/String)

Jeg håper dette har vært en nyttig introduksjon til hvordan du kan kapitalisere strenger i Elm. Nå kan du bruke denne kunnskapen til å lage enda bedre og mer profesjonelle applikasjoner!