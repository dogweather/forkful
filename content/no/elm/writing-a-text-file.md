---
title:    "Elm: Å skrive en tekstfil"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive en tekstfil er en viktig del av enhver programmeringsoppgave. Det tillater oss å lagre og manipulere data på en enkel og strukturert måte. I denne blogginnlegget skal vi se nærmere på hvordan du kan skrive tekstfiler ved hjelp av Elm programmeringsspråket.

## Slik gjør du det
For å skrive en tekstfil i Elm, må vi først importere File modulen. Deretter kan vi bruke funksjonen `writeFile` for å opprette og skrive til en tekstfil. Enkelt sagt tar denne funksjonen inn en liste med tekstlinjer, og skriver hver linje til filen etterfulgt av et linjeskift. La oss se på et eksempel:

```
import File exposing (..)

-- Opprett og skriv til en tekstfil
writeFile "minTekstfil.txt" ["Dette er en linje", "Dette er en annen linje"]

-- Resultatet vil være en tekstfil med innholdet:
-- Dette er en linje
-- Dette er en annen linje
```

Vi kan også bruke funksjonen `appendFile` for å legge til tekst til en eksisterende fil. For å lese en tekstfil, kan vi bruke `readFile` som returnerer en linje om gangen som en `Maybe String` verdi.

## Dykk ned
Nå som vi har lært hvordan du kan skrive og lese tekstfiler, la oss se på noen mer avanserte funksjoner som File modulen tilbyr. Vi kan for eksempel bruke `directory` funksjonen til å få en liste over filer i en bestemt mappe. Eller `rename` funksjonen for å endre navnet på en fil.

Vi kan også bruke Elm's Native modul for å få tilgang til mer avanserte funksjoner som å endre filrettigheter og slette filer. Men husk at disse funksjonene bare kan brukes i Elm's Native modul, og ikke i nettlesere hvor Elm vanligvis kjører.

## Se også
- Offisiell File modul dokumentasjon: https://package.elm-lang.org/packages/elm/file/latest/
- Elm Native modul dokumentasjon: https://package.elm-lang.org/packages/elm/core/latest/Native
- Enkel tekstfilbehandling i Elm: https://dev.to/evancz/text-file-handling-in-elm-2k6c