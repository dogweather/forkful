---
title:                "Fish Shell: Konvertere en streng til små bokstaver"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å konvertere en streng til små bokstaver? Vel, det kan være en nyttig funksjon å ha i ditt programmeringsverktøy. Kanskje du ønsker å lage et program som automatisk konverterer innkommende brukerinput til små bokstaver, eller kanskje du trenger å sammenligne tekststrenger som ikke er nøyaktig t idsnev. Uansett årsak, så er det enkelt å gjøre med Fish Shell.

## Hvordan man gjør det

For å konvertere en streng til små bokstaver i Fish Shell, kan du bruke kommandoen `string tolower`. For eksempel:

```
Fish Shell: string tolower "FISH SHELL"
fish shell
```

Som du kan se, konverteres strengen "FISH SHELL" til "fish shell". Dette gjøres ved å bruke det innebygde funksjonskallet `tolower` som endrer hver bokstav i strengen til små bokstaver.

## Dypdykk

En interessant ting å merke seg er at `string tolower` også kan håndtere ikke-ASCII bokstaver, som å, ø og å. Dette gjør det til et kraftig verktøy i situasjoner der du trenger å håndtere forskjellige språk og tegnsett.

En annen ting å huske på er at `string tolower` bare endrer de bokstavene som allerede er i små bokstaver. Hvis strengen inneholder store bokstaver, tall eller spesialtegn, vil de ikke bli endret. Du kan også bruke `string toupper` for å konvertere en streng til store bokstaver.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/string.html#tolower)
- [Konverter en streng til små bokstaver i Python](https://www.programiz.com/python-programming/methods/string/lower)
- [UTF-8 og Unicode forklart](https://www.utf8.no)