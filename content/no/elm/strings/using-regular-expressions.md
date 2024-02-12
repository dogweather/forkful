---
title:                "Bruke regulære uttrykk"
aliases:
- /no/elm/using-regular-expressions.md
date:                  2024-02-03T19:16:48.307178-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke regulære uttrykk"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk (regex) i programmering er mønstre brukt for å matche kombinasjoner av tegn i strenger. I Elm, akkurat som i andre språk, bruker programmerere regex for oppgaver som å validere inndata, søke etter og erstatte tekst i strenger på grunn av deres fleksibilitet og effektivitet.

## Hvordan:
Elm har ikke innebygde regex-funksjoner i sitt kjernelager, noe som krever bruk av tredjepartsbiblioteker for disse operasjonene. Et av de populære valgene for å jobbe med regex er `elm/regex`. Du kan legge det til i prosjektet ditt ved hjelp av `elm install elm/regex`.

Her er hvordan du kan bruke `elm/regex` for noen vanlige oppgaver:

### 1. Å matche et mønster
For å sjekke om en streng matcher et mønster, kan du bruke `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Eksempel på bruk:
isAlphanumeric "Elm2023"     -- Utdata: True
isAlphanumeric "Elm 2023!"   -- Utdata: False
```

### 2. Finne alle treff
For å finne alle forekomster av et mønster i en streng, kan du bruke `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Eksempel på bruk:
getWords "Elm is fun!"  -- Utdata: ["Elm", "is", "fun"]
```

### 3. Erstatte tekst
For å erstatte deler av en streng som matcher et mønster, bruker du `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Eksempel på bruk:
replaceElmWithHaskell "Learning Elm is fun!"  
-- Utdata: "Learning Haskell is fun!"
```

I disse eksemplene brukes `Regex.fromString` for å kompilere et regex-mønster, der `\b` matcher ordbegrensninger, og `\w` matcher hvilken som helst ordkarakter. Håndter alltid `Maybe`-resultatet av `Regex.fromString` for å sikre deg mot ugyldige regex-mønstre, typisk ved å bruke `Maybe.withDefault`.
