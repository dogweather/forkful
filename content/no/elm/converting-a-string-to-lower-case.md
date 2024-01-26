---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-01-20T17:38:29.851051-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle store bokstaver i teksten til sine tilsvarende små bokstav-versjoner. Programmerere gjør dette for å standardisere tekst, som i søkealgoritmer, brukernavn eller ved sammenligning av strenger uten å være følsom for bokstavstørrelse.

## Hvordan:
Elm gjør dette enkelt med `String.toLower` funksjonen. Her er et eksempel på bruk:

```Elm
import String

-- Konverter en streng til små bokstaver
lowercaseString : String -> String
lowercaseString str =
  String.toLower str

-- Eksempel på bruk
main =
  String.toLower "Hei, Verden!"
-- Output: "hei, verden!"
```

## Dykk Ned:
I de fleste programmeringsspråk, inkludert Elm, har standardbiblioteket en innebygd funksjon for å konvertere strenger til små bokstaver. Historisk sett har denne funksjonaliteten vært viktig for å unngå problemer med case-sensitivitet, spesielt i tidlige dager av informatikk når datainnsamling var inkonsekvent. Alternativer til `String.toLower` kan inkludere manuell gjennomgang av hver tegnkode og konvertering, men dette er unødvendig komplekst i Elm hvor funksjonen håndterer flere språk og kanttilfeller som tyske umlauts eller tyrkiske spesielle tegn. Implementeringsdetaljer er skjult fra brukeren, men Elm benytter seg av Unicode-standarder for å sikre bred tegnstøtte.

## Se Også:
- Elm-dokumentasjon for `String.toLower`: [String.toLower](http://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Unicode case mapping FAQ: [Unicode FAQ](http://unicode.org/faq/casemap_charprop.html)
- Sammenligning av strenger i Elm: [String Comparison](https://package.elm-lang.org/packages/elm/core/latest/String#compare)
