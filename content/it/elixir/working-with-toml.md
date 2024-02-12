---
title:                "Lavorare con TOML"
aliases:
- it/elixir/working-with-toml.md
date:                  2024-01-26T04:20:47.089172-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Lavorare con TOML significa analizzare e generare dati TOML (Tom's Obvious, Minimal Language) utilizzando Elixir. I programmatori lo utilizzano per gestire i file di configurazione perché TOML è leggibile, facile da analizzare e si mappa bene a una struttura dati hash.

## Come fare:
Prima, aggiungi un parser TOML alle tue dipendenze mix. Questo esempio utilizza `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Leggere un file TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Per convertire dati Elixir in TOML:

```elixir
data = %{title: "Esempio TOML", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Output di esempio:

```elixir
"title = \"Esempio TOML\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Approfondimento
TOML è stato creato da Tom Preston-Werner, co-fondatore di GitHub, per essere utilizzato nei file di configurazione. È progettato per essere più semplice rispetto a XML e più conciso rispetto a YAML, mantenendo al contempo consistenza.

Le alternative includono file JSON, YAML e INI, ognuno con i propri compromessi in termini di leggibilità umana e compatibilità delle strutture dati. TOML eccelle nel rappresentare chiaramente dati tabellari e nel raggruppamento annidato dei dati.

In Elixir, la gestione di TOML dipende da librerie di decodifica e codifica, che trasformano le stringhe TOML in mappe Elixir e viceversa. L'analisi funziona abbinando le regole sintattiche di TOML e convertendole nei tipi di dati di Elixir. La codifica fa l'opposto mappando i tipi di dati di Elixir di nuovo in una sintassi TOML valida.

## Vedi Anche
- Linguaggio TOML: https://toml.io/en/
- Repository GitHub di `toml-elixir`: https://github.com/bitwalker/toml-elixir
- Dettagli del pacchetto Hex per `toml-elixir`: https://hex.pm/packages/toml_elixir
