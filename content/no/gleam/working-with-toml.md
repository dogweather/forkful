---
title:                "Jobbe med TOML"
date:                  2024-01-26T04:22:11.799884-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med TOML betyr å parse og generere TOML (Tom's Obvious, Minimal Language)-filer med kode. Programmerere bruker TOML for lett-å-lese konfigurasjonsfiler og data serialisering, takket være dens klare semantikk og kompatibilitet med konvensjonelle datatyper.

## Hvordan:
Gleam har ikke innebygd støtte for TOML, så du trenger et eksternt bibliotek. For eksempel:

```gleam
// Antatt at du har et TOML parsing bibliotek:
import toml/{Parser, Encoder}

// Parse TOML innhold
let toml_content = """
[eier]
navn = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Bruk den parsete dataen
match parsed {
  Ok(data) -> "Data parset successful!"
  Error(_) -> "Klarte ikke å parse data."
}

// Generer TOML innhold fra Gleam datastruktur
let data = #{
  "eier": #{
    "navn": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Eksempel på output:

```
Data parset successful!
```

## Dypdykk
TOML ble sluppet i 2013 av Tom Preston-Werner. Målet: å være mer leselig og enkel enn XML og mindre kompleks enn YAML for filkonfigurasjoner. Til tross for enkelheten, er den robust for strukturert data, og tilbyr eksplisitt og lett-forståelig syntaks. Alternativer inkluderer JSON, YAML, og INI, men TOMLs minimalistiske og klare syntaks vinner ofte for konfigurasjonsfiler. Å implementere TOML i Gleam involverer to hovedhandlinger: å parse TOML inn i native datastrukturer og serialisere native datastrukturer inn i TOML. De fleste TOML biblioteker for Erlang eller Elixir kan brukes i Gleam på grunn av dets samspill med BEAM-språk, noe som sikrer sømløs integrasjon innen Gleam-prosjekter.

## Se Også
- TOML språk spesifikasjoner: [https://toml.io/en/](https://toml.io/en/)
- En Erlang TOML parser: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML på GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
