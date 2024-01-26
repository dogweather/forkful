---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con CSV significa manipolare dati in formato "Comma-Separated Values", ovvero valori separati da virgole. Programmatori lo fanno per importare, analizzare, e manipolare grandi volumi di dati con struttura tabellare in modo facile e veloce.

## How to:
Elixir rende semplice lavorare con i CSV attraverso la libreria `CSV`. Ecco come leggere e scrivere CSV:

```elixir
# Aggiungi nel tuo mix.exs: {:csv, "~> 2.4"}
defmodule CSVExample do
  require CSV

  # Leggere CSV da una stringa o file
  def read_csv(content) do
    content
    |> CSV.decode(separator: ?;)
    |> Enum.to_list()
  end

  # Scrivere dati in CSV
  def write_csv(data) do
    data
    |> CSV.encode(separator: ?;)
    |> Enum.join()
  end
end

# Uso:
content = "nome;cognome;eta\nMario;Rossi;30\nLuca;Bianchi;25"
data = CSVExample.read_csv(content)
IO.inspect(data)

csv_output = CSVExample.write_csv(data)
IO.puts(csv_output)
```

Output di lettura:
```elixir
[["nome", "cognome", "eta"], ["Mario", "Rossi", "30"], ["Luca", "Bianchi", "25"]]
```

Output di scrittura:
```plaintext
nome;cognome;eta
Mario;Rossi;30
Luca;Bianchi;25
```

## Deep Dive
Il CSV è nato negli anni '70 ed è diventato uno standard de facto per il trasferimento di dati tabellari. Rispetto a JSON o XML, il CSV è più leggero e semplice da leggere per l'uomo, ma meno versatile. Alternativa comune in Elixir è la libreria `nimble_csv` che offre più personalizzazioni. Internamente, lavorare con CSV implica parsing sequenziale e gestione di escaping e quoting corretto.

## See Also
- [CSV Hex Package](https://hex.pm/packages/csv): Documentazione ufficiale del pacchetto CSV per Elixir.
- [Elixir School](https://elixirschool.com/en/): Lezioni su Elixir, inclusi esempi con CSV.
- [RFC 4180](https://tools.ietf.org/html/rfc4180): Standard formale del formato CSV.
