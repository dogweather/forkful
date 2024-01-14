---
title:                "Elm: Utskrift av feilsøkingsutdata"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å printe ut feilsøkingsinformasjon? Det er fordi feil og problemer er en uunngåelig del av programmering. Ved å skrive ut feilsøkingsinformasjon kan du enklere identifisere og løse eventuelle problemer i koden din.

## Slik gjør du

For å printe ut debug-utdata i Elm kan du bruke funksjonen `Debug.log` som tar inn en melding og en verdi som skal printes ut. Her er et eksempel:

```Elm
import Debug exposing (log)

main =
    let
        sum = 2 + 2
    in
    Debug.log "Summen er" sum
```

Dette vil printe ut følgende i konsollen: `Summen er 4`.

Det er også mulig å printe ut mer komplekse strukturer som lister og tupler ved å bruke funksjonen `Debug.toString` på verdien du ønsker å printe ut.

## Dykk dypere

Å printe ut debug-utdata kan være nyttig i ulike situasjoner. Du kan for eksempel bruke det til å se hva som blir lagret i variabler, eller for å sjekke om en funksjon returnerer det forventede resultatet. Det er også mulig å aktivere debug-utskrift for hele applikasjonen ved å bruke `Debug.watch` eller `Debug.todo` funksjonene.

## Se også

* [Elm Official Guide - Debugging](https://guide.elm-lang.org/debugging/)
* [Elm Guide - Debug Output](https://elm-lang.org/debugging#debug-output)
* [Elm Packages - Debug](https://package.elm-lang.org/packages/elm-lang/core/latest/Debug)