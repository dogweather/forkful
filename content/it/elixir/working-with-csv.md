---
title:                "Lavorare con i file csv"
html_title:           "Elixir: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

Che cos'è e perché utilizzare CSV in Elixir

## Che cos'è e perché utilizzare CSV?

CSV (Comma-Separated Values) è un formato di file comunemente utilizzato per archiviare e trasferire dati in modo strutturato. In sostanza, è un elenco di valori separati da virgole, dove ogni riga rappresenta un record e le colonne sono i diversi campi dei dati.

I programmatori spesso lavorano con CSV perché è uno dei formati più semplici per lo scambio di dati tra diverse applicazioni o sistemi. Inoltre, è supportato da molti strumenti e librerie di programmazione.

## Come fare:

```
Elixir CSV è una libreria nativa di Elixir, quindi non è necessario installarla separatamente. Per utilizzarla, è sufficiente aggiungerla al file di progetto mix.exs:

defp deps do
  [
    {:csv, "~> 2.3"}
  ]
end
```
Per leggere un file CSV, puoi utilizzare la funzione `CSV.read/1` passando il percorso del file come argomento. Ad esempio:

```
# File CSV di esempio: "books.csv"
author,title,genre
J.K. Rowling,Harry Potter and the Philosopher's Stone,Fantasy
Jane Austen,Pride and Prejudice,Romance
J.R.R. Tolkien,The Lord of the Rings,Fantasy
```

```
Elixir iex> require CSV
Elixir iex> data = CSV.read("books.csv")
[
  [
    "author",
    "title",
    "genre"
  ],
  [
    "J.K. Rowling",
    "Harry Potter and the Philosopher's Stone",
    "Fantasy"
  ],
  [
    "Jane Austen",
    "Pride and Prejudice",
    "Romance"
  ],
  [
    "J.R.R. Tolkien",
    "The Lord of the Rings",
    "Fantasy"
  ]
]
```

Per scrivere un file CSV, è possibile utilizzare la funzione `CSV.encode/1` passando una lista di liste contenente i dati da scrivere. Ad esempio:

```
Elixir iex> data = [
  ["name", "age", "profession"],
  ["John", "30", "Software Developer"],
  ["Jane", "28", "Graphic Designer"]
]

Elixir iex> CSV.encode(data, quote: "\"")
"name","age","profession"
"John","30","Software Developer"
"Jane","28","Graphic Designer"
```

È anche possibile specificare diverse opzioni come delimitatore, citazione, encoding e altro ancora nella funzione `CSV.encode/2`. Per maggiori informazioni, si consiglia di consultare la documentazione ufficiale della libreria CSV di Elixir.

## Approfondimento:

CSV è stato sviluppato negli anni '70 ed è stato ampiamente utilizzato da allora per la sua semplicità e compatibilità. Tuttavia, con l'aumentare della complessità dei dati, sono stati sviluppati altri formati di file più avanzati, come JSON e XML.

Elixir offre anche altre librerie per lavorare con dati strutturati, come ExJSON per la gestione di file JSON e Xmerl per la manipolazione di file XML.

La libreria CSV di Elixir è basata sulla libreria di parsing di CSV di Erlang, che a sua volta è basata sulla libreria del linguaggio di programmazione C di RFC 4180, un documento ufficiale che definisce il formato CSV.

## Vedi anche:

- [Documentazione ufficiale della libreria CSV di Elixir](https://hexdocs.pm/csv/CSV.html)
- [RFC 4180 che definisce il formato CSV](https://tools.ietf.org/html/rfc4180)