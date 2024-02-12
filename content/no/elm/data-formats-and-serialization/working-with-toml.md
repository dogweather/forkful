---
title:                "Jobbe med TOML"
aliases: - /no/elm/working-with-toml.md
date:                  2024-01-26T04:21:18.694397-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
TOML, forkortelse for Toms Obvious, Minimal Language, er et dataserialiseringspråk. Elm-programmerere bruker det til å håndtere konfigurasjonsdata fordi det er menneskelesbart og kartlegger pene par med nøkkel-verdi som trengs i applikasjoner.

## Hvordan:
Elm har ikke en innebygd TOML-parser, men du kan samarbeide med JavaScript eller bruke en fellesskapspakke. Her er hvordan du kan parse TOML ved å bruke en hypotetisk `elm-toml`-pakke:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

For dekoding av spesifikke verdier:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

Eksempel på utdata for `port` kan være `Ok 8080` hvis dekodingen lykkes.

## Dypdykk
TOML ble skapt av Tom Preston-Werner, medgrunnlegger av GitHub, som et enkelt språk for konfigurasjonsfiler. Det konkurrerer med YAML og JSON; TOMLs syntaks sikter på det beste fra begge verdener med fokus på å være lett for mennesker å lese og skrive.

I Elm, for å håndtere TOML, trenger du typisk å gå gjennom JavaScript-samarbeid, noe som kan være litt av en utfordring. Heldigvis er Elm-fellesskapet ressurssterkt, og det finnes flere tredjepartspakker. Den hypotetiske `elm-toml`-pakken ville sannsynligvis bruke Elms `Port` for å kommunisere med en JavaScript TOML-parser eller implementere parsingen direkte i Elm.

Den største utfordringen i Elm er at alt statisk types, så du må skrive egendefinerte dekodere for å håndtere forskjellige datastrukturer innen TOML, noe som kan være litt langtekkelig, men legger til sikkerhet.

## Se Også
For spesifikasjoner og mer info om TOML selv, sjekk ut [TOML](https://toml.io).
Hvis du leter etter en praktisk tilnærming til Elm og JavaScript-samarbeid, start med den offisielle veiledningen: [Elm Ports](https://guide.elm-lang.org/interop/ports.html).
For fellesskapspakker eller for å bidra, bla gjennom [Elm Packages](https://package.elm-lang.org/).
