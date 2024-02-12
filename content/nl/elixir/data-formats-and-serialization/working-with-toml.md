---
title:                "Werken met TOML"
aliases: - /nl/elixir/working-with-toml.md
date:                  2024-01-28T22:10:41.037037-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met TOML betekent het parseren en genereren van TOML (Tom's Obvious, Minimal Language) gegevens met Elixir. Programmeurs gebruiken het om configuratiebestanden te verwerken omdat TOML leesbaar, eenvoudig te parsen is en goed overeenkomt met een hash datastructuur.

## Hoe:
Voeg eerst een TOML-parser toe aan je mix-afhankelijkheden. Dit voorbeeld gebruikt `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Een TOML-bestand lezen:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Om Elixir-gegevens naar TOML te converteren:

```elixir
data = %{title: "TOML Voorbeeld", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Voorbeelduitvoer:

```elixir
"title = \"TOML Voorbeeld\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Diepduiken
TOML is gecreëerd door Tom Preston-Werner, mede-oprichter van GitHub, voor gebruik in configuratiebestanden. Het is ontworpen om eenvoudiger te zijn dan XML en beknopter dan YAML, terwijl het consistent blijft.

Alternatieven zijn onder andere JSON, YAML en INI-bestanden, elk met hun eigen afwegingen in menselijke leesbaarheid en compatibiliteit met datastructuren. TOML blinkt uit in het duidelijk vertegenwoordigen van tabelgegevens en het geneste groeperen van gegevens.

In Elixir is de omgang met TOML afhankelijk van decodeer- en encodeerbibliotheken, die TOML-strings transformeren in Elixir-maps en vice versa. Het parsen werkt door het matchen van de syntaxisregels van TOML en deze te converteren naar de gegevenstypen van Elixir. Encoderen doet het tegenovergestelde door de gegevenstypen van Elixir terug te mappen naar geldige TOML-syntax.

## Zie Ook
- TOML-taal: https://toml.io/en/
- `toml-elixir` GitHub-repository: https://github.com/bitwalker/toml-elixir
- Hex-pakketdetails voor `toml-elixir`: https://hex.pm/packages/toml_elixir
