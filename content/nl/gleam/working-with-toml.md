---
title:                "Werken met TOML"
date:                  2024-01-28T22:10:52.504028-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met TOML betekent het parsen en genereren van TOML (Tom's Obvious, Minimal Language) bestanden met code. Programmeurs gebruiken TOML voor gemakkelijk te lezen configuratiebestanden en data-serialisatie, dankzij de duidelijke semantiek en compatibiliteit met conventionele datatypen.

## Hoe:
Gleam heeft geen ingebouwde ondersteuning voor TOML, dus je hebt een externe bibliotheek nodig. Bijvoorbeeld:

```gleam
// Uitgaande dat je een TOML-parsingbibliotheek hebt:
import toml/{Parser, Encoder}

// Parse TOML-inhoud
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Gebruik de geparseerde data
match parsed {
  Ok(data) -> "Data succesvol geparset!"
  Error(_) -> "Mislukt om data te parsen."
}

// Genereer TOML-inhoud vanuit Gleam datastructuur
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Voorbeelduitvoer:

```
Data succesvol geparset!
```

## Diepgaande Verkenning
TOML is in 2013 uitgebracht door Tom Preston-Werner. Het doel: leesbaarder en eenvoudiger zijn dan XML en minder complex dan YAML voor bestandsconfiguraties. Ondanks de eenvoud is het robuust voor gestructureerde data, en biedt het een expliciete en makkelijk te begrijpen syntaxis. Alternatieven zijn onder andere JSON, YAML, en INI, maar de minimalistische en duidelijke syntaxis van TOML wint vaak voor configuratiebestanden. TOML implementeren in Gleam omvat twee hoofdacties: TOML parsen naar inheemse datastructuren en inheemse datastructuren serialiseren naar TOML. De meeste TOML-bibliotheken voor Erlang of Elixir kunnen gebruikt worden in Gleam vanwege de interoperabiliteit met BEAM-talen, wat zorgt voor een naadloze integratie binnen Gleam-projecten.

## Zie Ook
- TOML taalspecificaties: [https://toml.io/en/](https://toml.io/en/)
- Een Erlang TOML parser: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML op GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
