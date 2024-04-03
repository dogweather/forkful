---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.269618-07:00
description: "Lavorare con JSON implica analizzare le stringhe formattate JSON in\
  \ strutture dati che Elixir pu\xF2 manipolare e serializzare le strutture dati di\
  \ Elixir di\u2026"
lastmod: '2024-03-13T22:44:43.105874-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con JSON implica analizzare le stringhe formattate JSON in strutture\
  \ dati che Elixir pu\xF2 manipolare e serializzare le strutture dati di Elixir di\
  \ nuovo in stringhe JSON."
title: Lavorare con JSON
weight: 38
---

## Cosa & Perché?

Lavorare con JSON implica analizzare le stringhe formattate JSON in strutture dati che Elixir può manipolare e serializzare le strutture dati di Elixir di nuovo in stringhe JSON. Questo è essenziale per lo sviluppo web, le API e i file di configurazione, poiché JSON è un formato di scambio dati leggero, basato su testo, indipendente dalla lingua, ampiamente utilizzato per la sua semplicità e leggibilità umana.

## Come fare:

In Elixir, puoi utilizzare la libreria `Jason`, una scelta popolare per l'analisi e la generazione di JSON. Prima, aggiungi `Jason` alle dipendenze del tuo progetto in `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Poi, esegui `mix deps.get` per recuperare la dipendenza.

### Analizzare il JSON:
Per convertire una stringa JSON in strutture dati Elixir:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Output: %{"name" => "John", "age" => 30}
```

### Generare JSON:
Per convertire una mappa Elixir in una stringa JSON:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Output: {"age":25,"name":"Jane"}
```

### Lavorare con Structs:
Per codificare una struct Elixir, devi implementare il protocollo `Jason.Encoder` per la tua struct. Ecco un esempio:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Output: {"age":28,"name":"Mike"}
```

Questo approccio semplice ti aiuterà a iniziare a integrare l'elaborazione di JSON nelle tue applicazioni Elixir, facilitando lo scambio di dati in vari ambienti di programmazione.
