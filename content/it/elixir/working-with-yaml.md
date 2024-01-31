---
title:                "Lavorare con YAML"
date:                  2024-01-19
simple_title:         "Lavorare con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato per dati umano-leggibili, usato per configurazioni o dati interscambiabili. Gli sviluppatori Elixir lo sfruttano per facilità di lettura e compatibilità tra varie tecnologie.

## How to:
Per lavorare con YAML in Elixir, usi la libreria `YamlElixir`. Ecco come leggere e scrivere in YAML:

```elixir
# Aggiungi `yaml_elixir` nel tuo mix.exs
defp deps do
  [
    {:yaml_elixir, "~> 2.8"}
  ]
end

# Uso per leggere YAML
{ok, data} = YamlElixir.read_from_string("""
name: Fulvio
role: Developer
languages:
  - Elixir
  - Ruby
""")

IO.inspect(data)
# Produce: %{"name" => "Fulvio", "role" => "Developer", "languages" => ["Elixir", "Ruby"]}

# Uso per scrivere in YAML
map = %{
  "name" => "Fulvio",
  "role" => "Developer",
  "languages" => ["Elixir", "Ruby"]
}

yaml = YamlElixir.write_to_string(map)
IO.puts yaml
# Produce:
# ---
# languages:
#   - Elixir
#   - Ruby
# name: Fulvio
# role: Developer
```

## Deep Dive
YAML nasce nel 2001 come alternativa a XML, facile da scrivere e leggere per umani. Per Elixir, `YamlElixir` è scelta comune, ma esistono anche librerie alternative come `yamerl` e `ex_yaml`. Internamente, `YamlElixir` si appoggia ad `erlang-yaml` che si basa su libyaml per parsing e dumping efficienti.

## See Also
- [YamlElixir](https://hex.pm/packages/yaml_elixir) su Hex
- Documentazione libyaml: [LibYAML](https://pyyaml.org/wiki/LibYAML)
  
Ricorda di sempre consultare la documentazione delle dipendenze per eventuali aggiornamenti o cambiamenti nelle versioni più recenti.
