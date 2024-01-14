---
title:                "Elm: Sletting av tegn som matcher et mønster"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster kan være en viktig del av enhver programmeringsoppgave. Det kan hjelpe deg med å rydde opp i tekststrenger og finne feil eller uønskede tegn.

## Hvordan

For å slette tegn som matcher et mønster i Elm kan du bruke funksjonen `String.filter`. Denne funksjonen tar inn en funksjon som parameter, som bestemmer hvilke tegn som skal slettes. La oss se på et eksempel:

```Elm
import String exposing (..)

text = "Hei, verden!"

filteredText = filter (\c -> c /= 'e') text

-- Utdata: "Hi, vrdn!"
```

Her har vi et enkelt eksempel der vi fjerner alle små bokstaver "e" fra en tekststreng. Ved å bruke `filter` funksjonen og sammenligne hvert enkelt tegn med "e", kan vi enkelt slette dem fra strengen.

En annen nyttig funksjon for å slette tegn basert på et mønster er `String.replace`. Denne funksjonen tar inn to tekststrenger som parametere og erstatter alle forekomster av den første strengen med den andre. La oss se på et annet eksempel:

```Elm
import String exposing (..)

text = "Den raske reven hoppet over den late hunden"

replacedText = replace "en" "e" text

-- Utdata: "Dn rask rev hoppet over d lat hund"
```

Her erstatter vi alle forekomster av "en" med "e" i teksten vår, og vi ender opp med en forenklet versjon av setningen uten at mening eller grammatikk er påvirket.

## Dypdykk

Nå som vi har sett på noen enkle eksempler på hvordan du kan slette tegn som matcher et mønster, kan vi også utforske mer avanserte tilnærminger. Ett av disse er å bruke regulære uttrykk i kombinasjon med `String.split` og `String.join` for å slette deler av en tekststreng som matcher et mønster. For eksempel:

```Elm
import String exposing (..)
import Regex exposing (..)

text = "Jeg har 10 epler, men bare 5 appelsiner"

numbersOnly = text
    |> split (regex "[^0-9]+")
    |> join ""

-- Utdata: "105"
```

I dette tilfellet bruker vi først `split` funksjonen til å splitte teksten vår basert på alle ikke-numeriske tegn. Deretter bruker vi `join` funksjonen for å sette sammen det som er igjen, som bare er tallene i teksten vår.

## Se også

- [Elm Offisiell Dokumentasjon](https://elm-lang.org/docs)
- [String modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Regex modulen](https://package.elm-lang.org/packages/elm/core/latest/Regex)