---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:41.037037-07:00
description: 'Hoe: Voeg eerst een TOML-parser toe aan je mix-afhankelijkheden. Dit
  voorbeeld gebruikt `toml-elixir`.'
lastmod: '2024-03-13T22:44:50.485143-06:00'
model: gpt-4-0125-preview
summary: Voeg eerst een TOML-parser toe aan je mix-afhankelijkheden.
title: Werken met TOML
weight: 39
---

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
