---
date: 2024-01-26 01:10:43.320879-07:00
description: "Hvordan: Her er en bit med Elm-kode med en enkel funksjon for \xE5 hilse\
  \ p\xE5 en bruker."
lastmod: '2024-03-13T22:44:40.714664-06:00'
model: gpt-4-1106-preview
summary: "Her er en bit med Elm-kode med en enkel funksjon for \xE5 hilse p\xE5 en\
  \ bruker."
title: Organisering av kode i funksjoner
weight: 18
---

## Hvordan:
Her er en bit med Elm-kode med en enkel funksjon for å hilse på en bruker:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Hei, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

Kjør den, og du får utdata: "Hei, Casey!"

Nå, la oss si at du vil legge til mer personalisering. Utdrag mer funksjonalitet!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Hallais" userName

main =
    text (personalGreeting "Casey")
```

Nå, når du kjører det: "Hallais, Casey!" Magi? Nei, bare funksjoner som gjør tingene sine.

## Dypdykk
I gamle dager var kode ofte en lang sekvens av instruksjoner (tenk spaghetti-kode). Det var et mareritt å vedlikeholde. Så kom strukturert programmering, og med det, funksjoner. Elm, som sine forgjengere innen funksjonell programmering, er tungt avhengig av funksjoner for organisering.

Du kan nøste funksjoner, og skape lukninger, eller holde dem rene for enkelthetens skyld. Elm oppmuntrer det siste: rene funksjoner med godt definerte innganger og utganger, noe som fører til lettere feilsøking og testing.

Elm-funksjoner kan også være av høyere orden, noe som betyr at de kan akseptere eller returnere andre funksjoner. Dette åpner en verden av komponerbarhet. Men, i motsetning til noen andre språk, har ikke Elm funksjonsoverlasting; hver funksjon må ha et unikt navn.

I tillegg pålegger Elm et sterkt statisk typsystem som ikke bare sjekker typene, men også inferrerer dem, og reduserer malingskode.

Sammenlignet med alternativer som prosedyre- eller objektorientert kodeorganisasjon i andre språk, legger Elm vekt på enkelhet og forutsigbarhet. Elm har ikke objekter eller klasser. Du organiserer kode med funksjoner og moduler i stedet for klasser og instanser.

## Se også
For å grave dypere, sjekk ut disse ressursene:
- Elms offisielle veiledning om funksjoner: https://guide.elm-lang.org/core_language.html
- Elm-pakkedokumentasjon for mer komplekse funksjonseksempler: https://package.elm-lang.org/
- Lær om Elms typsystem, som spiller pent med funksjonsorganisering: https://elm-lang.org/docs/types
