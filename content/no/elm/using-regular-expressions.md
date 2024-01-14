---
title:                "Elm: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor bruke regulære uttrykk i Elm-programmering

Regulære uttrykk, eller regex, er et kraftig verktøy som kan gjøre det mye enklere å håndtere og manipulere tekst i programmering. Ved hjelp av regex kan du søke etter et spesifikt mønster i en tekststreng og deretter utføre forskjellige handlinger basert på dette mønsteret. Dette kan være nyttig i en rekke programmeringsscenarioer, for eksempel når du arbeider med brukerinput, behandling av tekstfiler eller dynamisk generering av tekst. 

## Slik bruker du regulære uttrykk i Elm

Å bruke regulære uttrykk i Elm er enkelt og intuitivt. For å komme i gang må du importere regex-modulen ved å legge til `import Regex` øverst i filen din. Deretter kan du bruke funksjonen `Regex.find` for å søke etter et spesifikt mønster i en tekststreng. Her er et eksempel som søker etter e-postadresser i en tekststreng og returnerer en liste over alle treff:

```
Elm
Regex.find (Regex.fromString "\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b") "Dette er en tekst med en e-postadresse: test@example.com"
```
Dette vil gi følgende utdata:
```
Just (List.fromList [{"match": "test@example.com","index": 39,"number": 0}])
```
Som du kan se, har vi brukt en regex-mønstretring som parameter til `Regex.fromString`-funksjonen for å søke etter e-postadresser, og deretter brukes resultatet som et argument i `Regex.find`-funksjonen. Det er også verdt å merke seg at `Regex.find` returnerer et Maybe-type, noe som betyr at det enten vil være en `Just` eller `Nothing` verdien, avhengig av om mønsteret ble funnet eller ikke.

## Dykk dypere inn i regulære uttrykk i Elm

Å lære seg regulære uttrykk krever noe tid og praksis, men det er vel verdt innsatsen. I tillegg til å søke etter mønstre, kan du også bruke regex til å erstatte tekst, validere brukerinput og mye mer. Du kan også legge til flagg for å gjøre søket ditt mer presist og effektivt, eller bruke gruppering for å isolere deler av mønsteret du ønsker å arbeide med. Å bli kjent med disse forskjellige funksjonene og teknikkene vil gjøre deg i stand til å utnytte regulære uttrykk fullt ut i din Elm programmering.

## Se også

- [Elm dokumentasjon for regex](https://package.elm-lang.org/packages/elm/regex/latest/)
- [En praktisk gjennomgang av regulære uttrykk i Elm](https://alexspeller.com/learn-elm/regex/)
- [En sammenligning av ulike regex-implementeringer](https://github.com/friedbrice/elm-comparison-regex)