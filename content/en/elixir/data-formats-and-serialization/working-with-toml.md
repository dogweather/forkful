---
date: 2024-01-25 03:39:29.433423-07:00
description: 'How to: First, add a TOML parser to your mix dependencies. This example
  uses `toml-elixir`.'
lastmod: '2024-03-13T22:44:59.803167-06:00'
model: gpt-4-1106-preview
summary: First, add a TOML parser to your mix dependencies.
title: Working with TOML
weight: 39
---

## How to:
First, add a TOML parser to your mix dependencies. This example uses `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Read a TOML file:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

To convert Elixir data to TOML:

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Sample output:

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Deep Dive
TOML was created by Tom Preston-Werner, co-founder of GitHub, for use in configuration files. It's designed to be more straightforward than XML and more concise than YAML while maintaining consistency.

Alternatives include JSON, YAML, and INI files, each with their trade-offs in human readability and data structure compatibility. TOML excels in clearly representing tabular data and nested grouping of data.

In Elixir, TOML handling depends on decoding and encoding libraries, which transform TOML strings into Elixir maps and vice versa. Parsing works by matching TOML's syntax rules and converting them into Elixir's data types. Encoding does the opposite by mapping Elixir's data types back to valid TOML syntax.

## See Also
- TOML Language: https://toml.io/en/
- `toml-elixir` GitHub repository: https://github.com/bitwalker/toml-elixir
- Hex package details for `toml-elixir`: https://hex.pm/packages/toml_elixir
