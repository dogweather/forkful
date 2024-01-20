---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Utskrift av feilsøkingsdata er teknikken som brukes til å vise informasjon for å spore kodeflyt og identifisere problemer. Programmerere bruker dette for å forstå og løse feil effektivt.

## Slik gjør du:

Bruk `Debug.log` funksjonen i Elm. Vi printer en melding og hvilken verdi vi ønsker å sjekke. Her er en enkel kode:

```Elm
module Main exposing (main)

import Browser
import Html exposing (..)

main =
    Html.text <|
        Debug.log "Vår debug melding" "Hei verden!"
```

Når du kjører dette, vil du se i konsollen din:
 
```
Vår debug melding:  "Hei verden!"
```
## Dypdykk 

Historisk sett har utskrift av feilsøkingsdata alltid vært en del av programmering, fra fysisk lesing av hullkort til moderne software debugging.

Som et alternativ i Elm, kan du bruke `Debug.todo` som vil kaste en feil med en egendefinert melding. Dette kan være nyttig når du utvikler og vet at en viss kodebit ikke har blitt implementert ennå.

Detaljert implementering av `Debug.log` tvinger programmet til å skrive ut en melding til konsollen sammen med verdien som blir sjekket. Det returnerer den verifiserte verdien uendret, slik at det kan brukes i hvilken som helst sammenheng i koden din.

## Se også 

For mer informasjon, sjekk følgende ressurser:
 
- Elm dokumentasjon på `Debug.log`: https://package.elm-lang.org/packages/elm/core/latest/Debug#log
- Artikkel om feilsøking i Elm: https://elmprogramming.com/debugging.html
- Inndypende blogg om hvordan bruke `Debug.log`: https://www.elm-tutorial.org/en-v01/03-basics/13-debugging-with-log.html