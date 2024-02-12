---
title:                "Lavorare con YAML"
date:                  2024-02-03T19:25:04.790479-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

YAML, acronimo di YAML Ain't Markup Language, è uno standard di serializzazione dei dati leggibile dall'uomo comunemente utilizzato per file di configurazione e scambio di dati tra linguaggi con strutture dati diverse. I programmatori lo utilizzano per la sua semplicità e per la sua capacità di rappresentare facilmente dati gerarchici complessi.

## Come fare:

Elixir non include supporto incorporato per YAML. Tuttavia, è possibile utilizzare librerie di terze parti come `yamerl` o `yaml_elixir` per lavorare con YAML. Qui, ci concentreremo su `yaml_elixir` per la sua facilità d'uso e le sue funzionalità complete.

Prima, aggiungi `yaml_elixir` alle tue dipendenze in mix.exs:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Poi, esegui `mix deps.get` per recuperare la nuova dipendenza.

### Leggere YAML

Dato un semplice file YAML, `config.yaml`, che si presenta così:

```yaml
database:
  adapter: postgres
  username: utente
  password: pass
```

Puoi leggere questo file YAML e convertirlo in una mappa di Elixir così:

```elixir
defmodule Config do
  def read do
    {:ok, contenuto} = YamlElixir.read_from_file("config.yaml")
    contenuto
  end
end

# Esempio d'uso
Config.read()
# Output: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "utente",
#     "password" => "pass"
#   }
# }
```

### Scrivere YAML

Per scrivere una mappa su un file YAML:

```elixir
defmodule ConfigWriter do
  def write do
    contenuto = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", contenuto)
  end
end

# Esempio d'uso
ConfigWriter.write()
# Questo creerà o sovrascriverà `new_config.yaml` con il contenuto specificato
```

Notare come `yaml_elixir` permetta una traduzione diretta tra file YAML e strutture dati di Elixir, rendendolo un'eccellente scelta per i programmatori Elixir che necessitano di lavorare con dati YAML.
