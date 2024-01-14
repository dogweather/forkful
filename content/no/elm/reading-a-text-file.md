---
title:                "Elm: Lese en tekstfil"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese og behandle tekstfiler er en vanlig oppgave i programmering, enten det er for å importere data eller for å håndtere brukerinput. I denne bloggposten vil vi se nærmere på hvordan vi kan bruke Elm for å lese en tekstfil og prosessere innholdet.

## Hvordan

Først må vi importere `Text` modulen i vårt Elm-program. Deretter kan vi bruke funksjonen `File.toText` for å lese en tekstfil og lagre innholdet som en `String` i en `Result`-verdi. Her er et eksempel på hvordan dette kan gjøres:

```
Elm

import Text exposing (..)
import File
    exposing
        ( Error
        , Ok
        )

fileContent : Result Error String
fileContent =
    File.toText "minTekstfil.txt"
```

Dette vil gi oss innholdet i tekstfilen `minTekstfil.txt` lagret i variabelen `fileContent`. Nå kan vi jobbe med innholdet på samme måte som vi ville gjort med en vanlig tekststreng.

## Dypdykk

Når vi leser en tekstfil i Elm, vil teksten bli lagret som en enkel `String`. Det betyr at vi kan bruke alle de vanlige funksjonene for strenger for å behandle teksten videre, for eksempel `String.split`, `String.contains` eller `String.trim`.

Vi kan også implementere mer komplekse logikk for å håndtere spesifikke formater eller manipulere data på en spesifikk måte. Det viktige er å få teksten fra tekstfilen inn i vårt Elm-program, så kan vi jobbe med den på samme måte som vi ville gjort med en hvilken som helst annen tekststreng.

## Se også

- [Elm tekstmodulen dokumentasjon](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm File modulen dokumentasjon](https://package.elm-lang.org/packages/elm/file/latest/File)