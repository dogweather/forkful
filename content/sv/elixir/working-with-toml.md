---
title:                "Att arbeta med TOML"
date:                  2024-01-26T04:21:02.619679-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med TOML innebär att tolka och generera TOML-data (Tom's Obvious, Minimal Language) med hjälp av Elixir. Programmerare använder det för att hantera konfigurationsfiler eftersom TOML är lättläst, enkelt att tolka och mappar bra till en hashdatatstruktur.

## Hur:
Börja med att lägga till en TOML-tolk i dina mix-beroenden. Det här exemplet använder `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Läs en TOML-fil:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

För att konvertera Elixir-data till TOML:

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Exempelutskrift:

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Djupdykning
TOML skapades av Tom Preston-Werner, medgrundare av GitHub, för användning i konfigurationsfiler. Det är utformat för att vara mer rakt på sak än XML och mer koncist än YAML samtidigt som det bibehåller konsekvens.

Alternativ inkluderar JSON, YAML och INI-filer, var och en med sina avvägningar i människoläsbarhet och datastrukturkompatibilitet. TOML utmärker sig i att tydligt representera tabulär data och nästlade grupperingar av data.

I Elixir beror hanteringen av TOML på bibliotek för avkodning och kodning, vilka transformerar TOML-strängar till Elixir-kartor och tvärtom. Tolkningen fungerar genom att matcha TOML:s syntaxregler och omvandla dem till Elixirs datatyper. Kodningen gör motsatsen genom att mappa Elixirs datatyper tillbaka till giltig TOML-syntax.

## Se även
- TOML-språket: https://toml.io/en/
- `toml-elixir` GitHub-repositorium: https://github.com/bitwalker/toml-elixir
- Hex-paketdetaljer för `toml-elixir`: https://hex.pm/packages/toml_elixir
