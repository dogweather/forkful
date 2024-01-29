---
title:                "Werken met YAML"
date:                  2024-01-28T22:11:33.783195-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
YAML, "YAML Ain't Markup Language", is een voor mensen leesbare data-serialisatiestandaard. Programmeurs gebruiken het voor configuratiebestanden, gegevensuitwisseling tussen talen en omdat het leesbaarder is dan JSON of XML.

## Hoe te:
Gleam heeft momenteel geen ingebouwde YAML parsers of bibliotheken, voor zover mijn laatste update. Je zou typisch YAML in Gleam parsen door te leunen op Erlang-functies dankzij de compatibiliteit van Gleam met het ecosysteem van Erlang. Laten we een Erlang-bibliotheek gebruiken en deze vanuit Gleam aanroepen.

Voeg eerst de Erlang YAML-bibliotheek toe aan `rebar.config`:

```erlang
{deps, [yaml]}.
```

Hier is hoe je de Erlang-bibliotheek vanuit Gleam kunt aanroepen:

```rust
external fn parse(String) -> Result(Tuple(tuple(atom(), String)), Nil) =
  "yaml" "decode"

pub fn main() -> Result(Tuple(tuple(atom(), String)), Nil) {
  let yaml_data = "greeting: hello"
  parse(yaml_data)
}
```

Een voorbeelduitvoer kan er als volgt uitzien:

```elixir
Ok(#(ok, [{greeting, "hallo"}]))
```

## Diepere Duik
YAML is uitgebracht in 2001 en wordt vaak gebruikt waar menselijke leesbaarheid belangrijk is. Het is niet altijd de standaard voor data-serialisatie; JSON en XML worden ook veel gebruikt. Echter, de eenvoud van YAML maakt het ideaal voor configuratiebestanden of eenvoudige datastructuren.

Alternatieven kunnen de ingebouwde `:yamerl` parser van Elixir zijn, en in Gleam kunt u soortgelijke taken mogelijk afhandelen met JSON met de `gleam/json` bibliotheek. Wat betreft de implementatie, wanneer je met YAML werkt in Gleam, maak je gebruik van het bredere BEAM-ecosysteem - het is deze interoperabiliteit die het mogelijk maakt om YAML te parsen zonder dat een specifieke Gleam-bibliotheek nodig is.

## Zie Ook
- YAML-specificatie: https://yaml.org/spec/1.2/spec.html
- Erlang `yaml` bibliotheek: https://hex.pm/packages/yaml
- Documentatie van Gleam’s JSON-bibliotheek: https://hexdocs.pm/gleam_stdlib/gleam/json/
