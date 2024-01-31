---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein Format für Datenstrukturen, ähnlich wie JSON, aber menschenlesbarer. Programmierer nutzen es oft für Konfigurationsdateien und Datenaustausch, weil es einfach zu schreiben und zu lesen ist.

## How to:
### YAML in Elixir lesen:
```elixir
# Abhängigkeit in mix.exs: 
# {:yamerl, "~> 0.8"}

defmodule Example do
  def read_yaml do
    {:ok, yaml} = File.read("config.yml")
    :yamerl_constr.string(yaml)
  end
end

# Verwendung:
Example.read_yaml()
# => [{:ok, [deine_yaml_daten_hier]}]
```

### YAML in Elixir schreiben:
```elixir
# YAML-Writer gibt es nicht direkt in Elixir, aber du kannst Jason
# für JSON nutzen und dann zu YAML konvertieren (mit anderen Tools).

data = %{name: "Max", age: 28, languages: ["Elixir", "Ruby"]}
json_string = Jason.encode!(data)

# Konvertierung nach YAML außerhalb von Elixir durchführen
```

## Deep Dive
YAML, "YAML Ain't Markup Language", wurde Anfang der 2000er als einfacheres Format gegenüber XML entwickelt. Manchmal gilt JSON als Alternative zu YAML, besonders wenn Performance zählt, aber YAML bietet bessere Lesbarkeit für komplexe Konfigurationen. Die `yamerl` Bibliothek ermöglicht das Parsen von YAML in Elixir, für das Schreiben muss man meist auf externe Tools zurückgreifen, da es aktuell keine reine Elixir-Lösung gibt.

## See Also
YAML-Spezifikation: https://yaml.org/spec/1.2/spec.html  
`yamerl` Hex Paket: https://hex.pm/packages/yamerl  
Elixir's offizielle Website: https://elixir-lang.org/  
JSON in Elixir mit `Jason`: https://hex.pm/packages/jason
