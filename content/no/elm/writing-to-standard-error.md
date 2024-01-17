---
title:                "Skriving til standardfeil"
html_title:           "Elm: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Skriver til standardfeil betyr å sende en beskjed eller feilkode til et spesielt utgangspunkt i programmet som kalles "standard error". Dette er en viktig måte for programmerere å kommunisere feil og gjøre det enklere å feilsøke og debugge problemer.

Hvordan:
Mange programmeringsspråk, inkludert Elm, har en standardfunksjon for å skrive til standardfeil. Dette gjøres ved å bruke funksjonen "Debug.log" og gi den en feilkode som et argument, for eksempel "Debug.log "Feil ved å lese fil". Da vil feilkode og eventuelle tilhørende verdier sendes til standardfeil. Et eksempel på kode og output kan se ut som dette:

```Elm
import Debug

Debug.log "Feilkode" 42

-- output:
-- Feilkode: 42
```

Dypere dykk:
Skriver til standardfeil har vært en vanlig praksis blant programmere i lang tid. I eldre programmeringsspråk ble det ofte gjort ved å bruke en funksjon som het "printf" eller "print", men i nyere språk som Elm er det en spesiell funksjon som er dedikert til dette formålet. Dette gjør det enklere for programmere å finne feil og feilsøke problemene sine.

Se også:
For å lære mer om å skrive til standardfeil i Elm, kan du sjekke ut den offisielle Elm dokumentasjonen om Debug-modulen: https://package.elm-lang.org/packages/elm/core/latest/Debug.