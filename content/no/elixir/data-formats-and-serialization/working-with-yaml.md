---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:14.860067-07:00
description: "Hvordan: Elixir inkluderer ikke innebygd st\xF8tte for YAML. Imidlertid\
  \ kan du bruke tredjepartsbiblioteker som `yamerl` eller `yaml_elixir` for \xE5\
  \ arbeide\u2026"
lastmod: '2024-03-13T22:44:40.463607-06:00'
model: gpt-4-0125-preview
summary: "Elixir inkluderer ikke innebygd st\xF8tte for YAML."
title: Arbeider med YAML
weight: 41
---

## Hvordan:
Elixir inkluderer ikke innebygd støtte for YAML. Imidlertid kan du bruke tredjepartsbiblioteker som `yamerl` eller `yaml_elixir` for å arbeide med YAML. Her vil vi fokusere på `yaml_elixir` for dets brukervennlighet og omfattende funksjoner.

Først, legg til `yaml_elixir` i avhengighetene dine i mix.exs:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Deretter kjør `mix deps.get` for å hente den nye avhengigheten.

### Lese YAML
Gitt en enkel YAML-fil, `config.yaml`, som ser slik ut:

```yaml
database:
  adapter: postgres
  brukernavn: bruker
  passord: pass
```

Du kan lese denne YAML-filen og konvertere den til et Elixir-kart slik:

```elixir
defmodule Config do
  def read do
    {:ok, innhold} = YamlElixir.read_from_file("config.yaml")
    innhold
  end
end

# Eksempel på bruk
Config.read()
# Utdata: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "brukernavn" => "bruker",
#     "passord" => "pass"
#   }
# }
```

### Skrive YAML
For å skrive et kart tilbake til en YAML-fil:

```elixir
defmodule ConfigWriter do
  def write do
    innhold = %{
      database: %{
        adapter: "mysql",
        brukernavn: "root",
        passord: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", innhold)
  end
end

# Eksempel på bruk
ConfigWriter.write()
# Dette vil opprette eller overskrive `new_config.yaml` med det spesifiserte innholdet
```

Legg merke til hvordan `yaml_elixir` tillater en enkel oversettelse mellom YAML-filer og Elixir-datastrukturer, noe som gjør det til et utmerket valg for Elixir-programmerere som trenger å arbeide med YAML-data.
