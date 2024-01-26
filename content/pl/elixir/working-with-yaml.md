---
title:                "Praca z yaml"
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML to format zapisu danych, czyściocha i czytelny. Programiści używają go do konfiguracji, bo jest prostszy do odczytu i edycji niż JSON czy XML.

## How to:
Elixir współpracuje z YAML za pomocą pakietu `yaml_elixir`. Poniżej przykład.

```elixir
# Dołącz zależność w mix.exs
defp deps do
  [{:yaml_elixir, "~> 2.5"}]
end

# Użycie YamlElixir do przeczytania pliku YAML
{:ok, content} = YamlElixir.read_from_file("konfiguracja.yaml")
IO.inspect(content)

# Przykładowa zawartość konfiguracja.yaml
#---
# developer: "Jan Kowalski"
# environment: "development"

# Wyjście
# %{"developer" => "Jan Kowalski", "environment" => "development"}
```

## Deep Dive
YAML pojawił się na początku lat 2000 jako ulepszona forma serializacji danych z firmami takimi jak Basecamp pchając jego adaptację. Alternatywami dla YAML są JSON, który jest bardziej rozbudowany lub TOML, często używany w Rust. YAML w Elixirze jest implementowany poprzez biblioteki jak `yaml_elixir`, które korzystają z C-biblioteki `libyaml` do przetwarzania danych.

## See Also
- YAML specyfikacja: https://yaml.org/spec/1.2.2/
- Repozytorium `yaml_elixir` na Hex: https://hex.pm/packages/yaml_elixir
- Dokumentacja Elixir: https://elixir-lang.org/docs.html
- Basecamp: https://basecamp.com/about/history
