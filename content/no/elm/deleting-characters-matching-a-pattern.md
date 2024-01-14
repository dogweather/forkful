---
title:    "Elm: Sletting av tegn som matcher et mønster"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster kan være nyttig når du jobber med strenger i Elm. Dette kan bidra til å organisere og filtrere data på en effektiv måte.

## Hvordan du gjør det

For å slette tegn som matcher et mønster, kan du bruke `String.filter` funksjonen i Elm. Dette tar imot en funksjon som returnerer en Bool verdi, og fjerner alle tegn som returnerer True fra funksjonen.

```Elm
import String exposing (filter)

filter (\c -> c /= "a") "Elm is amazing" -- "Elm is mzing" 
```

I dette eksempelet, vil alle "a" karakterer bli fjernet fra strengen "Elm is amazing". Du kan også bruke mer kompliserte funksjoner for å slette tegn som passerer et gitt mønster.

```Elm
import String exposing (filter)

hasEvenIndex c index =
    index % 2 == 0

filter (\c -> hasEvenIndex c 0) "Elm is amazing" -- "m i szn"
```

Her bruker vi en hjelpefunksjon `hasEvenIndex` som returnerer True for tegn med partall indeks. Dette vil fjerne alle tegn på partallsindeks fra strengen "Elm is amazing".

## Dypdykk

Det er verdt å merke seg at `String.filter` funksjonen vil returnere en ny streng i stedet for å endre den originale strengen. Dette gjør at operasjonen er trygg og ikke forandrer dataen din.

Du kan også bruke `String.replace` funksjonen for å erstatte tegn som matcher et gitt mønster med et annet tegn. Dette kan være nyttig hvis du ønsker å bytte ut spesifikke tegn i en streng.

## Se også

- [Elm Docs: String.filter](https://package.elm-lang.org/packages/elm-lang/core/3.0.0/String#filter)
- [Elm Docs: String.replace](https://package.elm-lang.org/packages/elm-lang/core/3.0.0/String#replace)