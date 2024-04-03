---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:39.845347-07:00
description: "Lavorare con file CSV (Valori Separati da Virgola) implica la lettura\
  \ e la scrittura di dati su questi file, una necessit\xE0 comune per compiti che\u2026"
lastmod: '2024-03-13T22:44:43.106949-06:00'
model: gpt-4-0125-preview
summary: "Lavorare con file CSV (Valori Separati da Virgola) implica la lettura e\
  \ la scrittura di dati su questi file, una necessit\xE0 comune per compiti che richiedono\
  \ import/export di dati o semplici soluzioni di archiviazione."
title: Lavorare con i CSV
weight: 37
---

## Come fare:
Elixir, con il suo potente pattern matching e il supporto per il pipeline, può gestire i file CSV in modo efficiente, anche senza librerie di terze parti. Tuttavia, per necessità più avanzate, la libreria `nimble_csv` è una scelta rapida e diretta.

### Leggere un File CSV Senza Librerie Esterne
Puoi leggere e analizzare un file CSV utilizzando le funzioni incorporate di Elixir:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Esempio di utilizzo
CSVReader.read_file("data.csv")
# Output: [["Intestazione1", "Intestazione2"], ["ValoreRiga1Elemento1", "ValoreRiga1Elemento2"], ["ValoreRiga2Elemento1", "ValoreRiga2Elemento2"]]
```

### Scrivere in un File CSV
Similmente, per scrivere dati in un file CSV:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# Esempio di utilizzo
data = [["Intestazione1", "Intestazione2"], ["Valore1", "Valore2"], ["Valore3", "Valore4"]]
CSVWriter.write_to_file("output.csv", data)
# Crea output.csv con i dati formattati come CSV
```

### Usare `nimble_csv`
Per la gestione di CSV più complessi, `nimble_csv` offre un modo potente e flessibile di lavorare con i dati CSV. Prima di tutto, aggiungi `nimble_csv` alle tue dipendenze in `mix.exs` ed esegui `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

Analizzare dati CSV con `nimble_csv`:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# Esempio di utilizzo
MyCSVParser.parse("data.csv")
# L'output con nimble_csv può essere personalizzato in base alla definizione, ma in generale appare come una lista di liste o tuple a seconda di come configuri il tuo parser.
```

Scrivere dati CSV usando `nimble_csv` richiede di trasformare manualmente i tuoi dati in un formato adeguato e poi scriverli su un file, molto simile all'esempio di Elixir semplice ma sfruttando `nimble_csv` per generare righe CSV formattate correttamente.

Scegliendo l'approccio appropriato per la complessità del tuo compito, puoi gestire i file CSV in Elixir con grande flessibilità e potenza.
