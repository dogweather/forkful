---
title:                "Elm: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert i Elm, har du kanskje støtt på behovet for å feilsøke og finne ut hva som skjer under kjøring av koden din. Det er derfor å oversette og utskrive debugging-informasjon til konsollen kan være en nyttig og effektiv måte å løse problemer på.

## Hvordan

Det å skrive ut debug output i Elm er en enkel prosess. Først må du importere `Debug` modulen i koden din:

```Elm
import Debug exposing (log)
```

Deretter kan du bruke `log` funksjonen til å skrive ut hvilken som helst verdi til konsollen:

```Elm
log "Hei!"  -- Skriver ut "Hei!"
```

Du kan også skrive ut mer komplekse verdier, som lister eller rekursiv datastrukturer:

```Elm
let navn = "John"
let alder = 30
let kjæledyr = ["Katt", "Hund"]
let person = { navn = navn, alder = alder, kjæledyr = kjæledyr }

log person  -- Skriver ut "{ navn = "John", alder = 30, kjæledyr = ["Katt", "Hund"] }"
```

Å skrive ut verdier til konsollen kan være spesielt nyttig når du jobber med løkker eller funksjoner, da det lar deg se verdien til variabler i forskjellige stadier av koden din.

## Deep Dive

Når du skriver ut debug output, kan du også spesifisere en "tag" som det første argumentet for `log` funksjonen. Dette kan være nyttig når du trenger å skille mellom forskjellige utskrifter.

```Elm
log "Tag" "Verdi" -- Skriver ut "Tag: Verdi"
```

I tillegg, i noen situasjoner, kan det å bruke `toString` funksjonen på en verdi være mer nyttig enn å bruke `log`. Dette vil konvertere verdiene til en lesbar streng, og kan hjelpe deg med å finne ut hva som skjer i forskjellige deler av koden din.

## Se også

- [Elm docs: Debug module](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm docs: toString function](https://package.elm-lang.org/packages/elm/core/latest/String#toString)