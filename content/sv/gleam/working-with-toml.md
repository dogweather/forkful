---
title:                "Att arbeta med TOML"
date:                  2024-01-26T04:22:00.854880-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jobba med TOML innebär att tolka och generera TOML (Tom's Obvious, Minimal Language) filer med kod. Programmerare använder TOML för lättlästa konfigurationsfiler och dataserialisering, tack vare dess klara semantik och kompatibilitet med konventionella datatyper.

## Hur man gör:
Gleam har inte inbyggt stöd för TOML, så du behöver ett externt bibliotek. Till exempel:

```gleam
// Antag att du har ett TOML-tolkningsbibliotek:
import toml/{Parser, Encoder}

// Tolka TOML-innehåll
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Använd den tolkade datan
match parsed {
  Ok(data) -> "Data tolkades framgångsrikt!"
  Error(_) -> "Misslyckades med att tolka data."
}

// Generera TOML-innehåll från Gleam-datastruktur
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Exempelutdata:

```
Data tolkades framgångsrikt!
```

## Fördjupning
TOML släpptes 2013 av Tom Preston-Werner. Målet: att vara mer läsbart och enklare än XML samt mindre komplext än YAML för filkonfigurationer. Trots enkelheten är det robust för strukturerade data, och erbjuder en uttrycklig och lättförståelig syntax. Alternativ inkluderar JSON, YAML och INI, men TOMLs minimalistiska och klara syntax vinner ofta för konfigurationsfiler. Att implementera TOML i Gleam innebär två huvudaktioner: tolkning av TOML till inhemska datastrukturer och serialisering av inhemska datastrukturer till TOML. De flesta TOML-bibliotek för Erlang eller Elixir kan användas i Gleam på grund av dess interoperabilitet med BEAM-språk, vilket säkerställer sömlös integration inom Gleam-projekt.

## Se även
- TOML-språkspecifikationer: [https://toml.io/en/](https://toml.io/en/)
- En Erlang TOML-tolkare: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML på GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)