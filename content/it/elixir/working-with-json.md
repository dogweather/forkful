---
title:                "Lavorare con JSON"
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

Lavorare con JSON significa manipolare il formato di scambio dati più popolare per applicazioni web. I programmatori lo fanno per la sua leggibilità e per facilitare l'interoperabilità tra sistemi differenti.

## How to:

Per lavorare con JSON in Elixir, puoi usare la libreria `Jason`. Assicurati di aggiungerla come dipendenza nel tuo `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Ecco come decodificare una stringa JSON in una mappa di Elixir:

```elixir
iex> json = "{\"chiave\": \"valore\"}"
iex> {:ok, data} = Jason.decode(json)
iex> data
%{"chiave" => "valore"}
```

Ora, codifichiamo una mappa Elixir in una stringa JSON:

```elixir
iex> mapa = %{"chiave" => "valore"}
iex> {:ok, json} = Jason.encode(mapa)
iex> json
"{\"chiave\":\"valore\"}"
```

## Deep Dive

JSON, acronimo di JavaScript Object Notation, nato nel 2001, è un sottoinsieme di JavaScript. Nonostante le sue origini, è indipendente dal linguaggio e molti linguaggi di programmazione, incluso Elixir, hanno librerie per manipolarlo. `Poison` è un'alternativa a `Jason` in Elixir, ma `Jason` è generalmente preferito per prestazioni migliori. Entrambi convertiscono dati JSON in strutture native di Elixir e viceversa, ma bisogna conoscere bene la specifica JSON e monitorare il comportamento con dati non validi o inaspettati.

## See Also

- Documentazione Elixir `Jason`: https://hexdocs.pm/jason/readme.html
- Specifica JSON: https://www.json.org/json-it.html
- Confronto tra `Jason` e `Poison`: https://elixirforum.com/t/jason-vs-poison/15307
- Guida introduttiva a JSON in JavaScript: https://developer.mozilla.org/it/docs/Learn/JavaScript/Objects/JSON
